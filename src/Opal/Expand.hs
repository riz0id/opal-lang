{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Opal.Expand (
  -- * Expand Monad
  Expand (Expand, unExpand),

  -- ** Expander Errors
  ExpandError (ExnResolveError, ExnApplicationToValue),

  -- ** Expander Context
  ExpandContext (..),

  -- ** Expander Store
  ExpandStore (..),

  -- * TODO
  runSyntaxEval,
  runSyntaxExpand,
  runExpand,

  -- * TODO
) where

import Control.Lens (over, set, view)

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (modify', state, gets)

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core (Expr, SExp (..))
import Opal.Core.Datum (Datum)
import Opal.Core.Datum qualified as Datum
import Opal.Core.Form qualified as Core.Form
import Opal.Core.Prim qualified as Core.Prim

import Opal.Expand.Core (
  flipsSyntax,
  indexTransformers,
  introBinding,
  introCoreBinds,
  newIntroScopeId,
  newUsageScopeId,
  nextPhase,
  parse,
  pruneSyntax,
  scopeStxIdt,
  scopeSyntax, resolveIdt,
 )
import Opal.Expand.Evaluate (exprEval)
import Opal.Expand.Monad
import Opal.Expand.Namespace (makeEmptyNamespace)
import Opal.Expand.Resolve (resolveName)
import Opal.Expand.Syntax (
  StxCtx,
  StxIdt (StxIdt),
  Syntax (StxAtom, StxBool, StxList, StxPair),
 )
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform)
import Opal.Expand.Transform qualified as Transform

import Opal.Parse (ParseError (..))
import Opal.Parse qualified as Parse

import Debug.Trace qualified as Debug
import Control.Monad.Error (catchError)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runSyntaxEval :: Syntax -> Either ExpandError Datum
runSyntaxEval stx =
  let ctx = makeExpandContext makeEmptyNamespace
      stw = makeExpandStore
   in runExpand ctx stw do
        let coreScope = ScopeId 0
        introCoreBinds
        stx' <- moduleExpand =<< scopeSyntax coreScope stx
        sexp <- parse (Parse.pSyntax stx')
        exprEval sexp

-- | TODO
--
-- @since 1.0.0
runSyntaxExpand :: Syntax -> Either ExpandError Syntax
runSyntaxExpand stx =
  let ctx = makeExpandContext makeEmptyNamespace
      stw = makeExpandStore
   in runExpand ctx stw do
        let coreScope = ScopeId 0
        introCoreBinds
        stx' <- scopeSyntax coreScope stx
        moduleExpand stx'

-- | TODO
--
-- @since 1.0.0
runExpand :: ExpandContext -> ExpandStore -> Expand a -> Either ExpandError a
runExpand ctx st0 mx =
  case unExpand mx ctx st0 of
    (# _, (# e | #) #) -> Left e
    (# _, (# | x #) #) -> Right x
{-# INLINE runExpand #-}

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
-- topLevelExpand :: Syntax -> Expand Syntax
-- topLevelExpand stx = do
--   _

-- | TODO
--
-- @since 1.0.0

-- | TODO
--
-- @since 1.0.0
-- moduleLevelExpand :: Syntax -> Expand Syntax
-- moduleLevelExpand stx =
--   local (set ctxMode ExpandModule) do
--     _

-- | TODO
--
-- @since 1.0.0
-- moduleExpand :: StxIdt -> [Syntax] -> Expand Syntax
-- moduleExpand idt stxs = _

-- | TODO
--
-- @since 1.0.0
-- bodyExpand :: [Syntax] -> ScopeId -> Expand [Syntax]
-- bodyExpand stxs sc = do

--   outside'sc <- newScopeId
--   inside'sc <- newScopeId

--   init'stxs <- for stxs \stx1 -> do
--     stx2 <- scopeSyntax sc stx1
--     stx3 <- scopeSyntax outside'sc stx2
--     scopeSyntax inside'sc stx3

--   _

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
syntaxExpand :: Syntax -> Expand Syntax
syntaxExpand stx = do
  ph <- asks ctx'phase
  let coreScope :: ScopeId
      coreScope = ScopeId 0
   in case Syntax.scope ph coreScope stx of
        StxBool ctx bool -> pure (StxBool ctx bool)
        StxPair ctx stx0 stx1 -> do
          stx0' <- syntaxExpand stx0
          stx1' <- syntaxExpand stx1
          pure (StxPair ctx stx0' stx1')
        StxAtom ctx atom -> stxIdtExpand (StxIdt ctx atom)
        StxList ctx stxs -> stxListExpand ctx stxs

-- | TODO
--
-- @since 1.0.0
stxIdtExpand :: StxIdt -> Expand Syntax
stxIdtExpand idt = do
  name <- resolveName idt
  form <- indexTransformers name
  case form of
    Transform.Var idt' ->
      pure idt'.syntax
    Transform.Dtm (Datum.Bool bool) -> do
      pure (StxBool idt.context bool)
    Transform.Dtm datum -> do
      let macro = idt.syntax
      applyTransformer (SExpVal datum) macro
    tfm -> do
      error ("stxIdtExpand: " ++ show tfm)

stxListExpand :: StxCtx -> [Syntax] -> Expand Syntax
stxListExpand ctx (StxAtom ctx' atom : stxs) = do
  idtApplicationExpand ctx (StxIdt ctx' atom) stxs
stxListExpand ctx stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx stxs')

--------------------------------------------------------------------------------

idtApplicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
idtApplicationExpand ctx idt stxs = do
  name <- resolveName idt
  form <- indexTransformers name
  case form of
    Transform.Core Core.Form.Quote -> do
      let quote = StxAtom idt.context "quote"
      pure (StxList ctx (quote : stxs))
    Transform.Core Core.Form.QuoteSyntax
      | [stx] <- stxs -> do
          let quote'syntax = StxAtom idt.context "quote-syntax"
          stx' <- pruneSyntax stx
          pure (StxList ctx [quote'syntax, stx'])
      | otherwise -> do
          throwError (ExnParseError (ExnParseSyntax stxs))
    Transform.Core Core.Form.Lambda
      | stx : body <- stxs -> do
          let lambda = StxAtom idt.context "lambda"
          args <- parse (Parse.pStxFormalIdts stx)
          (sc, body') <- lambdaBodyExpand args body
          scopeSyntax sc (StxList ctx (lambda : stx : body'))
      | otherwise -> do
          throwError (ExnParseError (ExnParseLambda stxs))
    Transform.Core Core.Form.If
      | [stx1, stx2, stx3] <- stxs -> do
          let if' = StxAtom idt.context "if"
          stx1' <- syntaxExpand stx1
          stx2' <- syntaxExpand stx2
          stx3' <- syntaxExpand stx3
          pure (StxList ctx [if', stx1', stx2', stx3'])
      | otherwise -> do
          throwError (ExnParseError (ExnParseIf idt.syntax stxs))
    Transform.Core Core.Form.Let
      | stx : body <- stxs -> do
          let let' = StxAtom idt.context "let"
          stx' <- letVarsExpand stx
          vars <- parse (Parse.pStxLetSyntaxBinds stx')
          (sc, body') <- lambdaBodyExpand (map fst vars) body
          scopeSyntax sc (StxList ctx (let' : stx' : body'))
      | otherwise -> do
          throwError (ExnParseError (ExnParseLet stxs))
    Transform.Core Core.Form.LetSyntax -> do
      (binds, body) <- parse (Parse.pStxLetSyntax stxs)
      letSyntaxExpand binds body
    Transform.Core Core.Form.DefineValue -> do
      pure (StxList ctx (idt.syntax : stxs))
    Transform.Core coreform -> do
      pure (error ("idtApplicationExpand: unhandled core form case: " ++ show coreform))
    Transform.Dtm (Datum.Prim prim) -> do
      let func = StxIdt idt.context (Core.Prim.toSymbol prim)
      applicationExpand ctx func stxs
    Transform.Dtm (Datum.Proc args body) -> do
      let macro = StxList ctx (idt.syntax : stxs)
      applyTransformer (SExpVal (Datum.Proc args body)) macro
    Transform.Dtm datum -> do
      throwError (ExnApplicationToValue datum stxs)
    Transform.Var func -> do
      applicationExpand ctx func stxs

moduleExpand :: Syntax -> Expand Syntax
moduleExpand (StxList ctx (StxAtom ctx' atom : stxs)) = do
  name <- resolveName (StxIdt ctx' atom)
  bind <- indexTransformers name
  case bind of
    Transform.Core Core.Form.Module -> do
      let module' = StxAtom ctx' "module"
      stxs' <- moduleBodyExpand stxs
      pure (StxList ctx (module' : stxs'))
    _ -> do
      let body = StxAtom ctx' atom : stxs
      body' <- moduleBodyExpand body
      pure (StxList ctx body')
moduleExpand (StxList ctx stxs) = do
  let !_ = Debug.trace (show stxs) ()
  stxs' <- moduleBodyExpand stxs
  pure (StxList ctx stxs')
moduleExpand stx = syntaxExpand stx

moduleBodyExpand :: [Syntax] -> Expand [Syntax]
moduleBodyExpand stxs = do
  (stxs', forms) <- modulePartialBodyExpand stxs
  local (set ctxTransformers forms) do
    (final, _) <- moduleDefnsBodyExpand stxs'
    pure final

modulePartialBodyExpand :: [Syntax] -> Expand ([Syntax], Map Name Transform)
modulePartialBodyExpand [] = do
  forms <- asks (view ctxTransformers)
  pure ([], forms)
modulePartialBodyExpand (StxList ctx (StxAtom ctx' atom : stxs) : rest) = do
  let idt = StxIdt ctx' atom

  bind <- catchError (fmap Just (resolveIdt idt)) \_ -> pure Nothing
  
  case bind of 
    Just (Transform.Core Core.Form.DefineSyntaxValue) -> do
      (name, defn) <- nextPhase do
        (name, stx') <- defineSyntaxValueExpand stxs
        sexp <- parse (Parse.pSyntax stx')
        defn <- exprEval sexp
        pure (name, defn)
      let add'transformer = Map.insert name (Transform.Dtm defn)
      local (over ctxTransformers add'transformer) do
        modulePartialBodyExpand rest
    _ -> do
      (rest', forms) <- modulePartialBodyExpand rest
      pure (StxList ctx (StxAtom ctx' atom : stxs) : rest', forms)
modulePartialBodyExpand (stx : stxs) = do
  (stxs', forms) <- modulePartialBodyExpand stxs
  pure (stx : stxs', forms)

moduleDefnsBodyExpand :: [Syntax] -> Expand ([Syntax], Map Name Transform)
moduleDefnsBodyExpand [] = do
  forms <- asks (view ctxTransformers)
  pure ([], forms)
moduleDefnsBodyExpand (StxList ctx (StxAtom ctx' atom : stxs) : rest) = do
  resolveIdt (StxIdt ctx' atom) >>= \case
    Transform.Core Core.Form.DefineValue -> do
      (name, idt, defn) <- defineValueBodyExpand stxs
      let add'transformer = Map.insert name (Transform.Var idt)
      local (over ctxTransformers add'transformer) do
        let decl = StxList ctx [StxAtom ctx' atom, idt.syntax, defn]
        (rest', forms) <- moduleDefnsBodyExpand rest
        pure (decl : rest', forms)
    Transform.Var idt -> do 
      stxs' <- traverse syntaxExpand stxs
      (rest', forms) <- moduleDefnsBodyExpand rest
      pure (StxList ctx (idt.syntax : stxs') : rest', forms)
    _ -> do
      stx' <- syntaxExpand (StxList ctx (StxAtom ctx' atom : stxs))
      (rest', forms) <- moduleDefnsBodyExpand rest
      pure (stx' : rest', forms)
moduleDefnsBodyExpand (stx : stxs) = do
  stx' <- syntaxExpand stx
  (stxs', forms) <- moduleDefnsBodyExpand stxs
  pure (stx' : stxs', forms)

applicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
applicationExpand ctx func stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx (func.syntax : stxs'))

--------------------------------------------------------------------------------

defineSyntaxValueExpand :: [Syntax] -> Expand (Name, Syntax)
defineSyntaxValueExpand [StxAtom ctx atom, stx] = do
  let idt = StxIdt ctx atom
  stx' <- syntaxExpand stx
  name <- introBinding idt
  pure (name, stx')
defineSyntaxValueExpand stxs = do
  pure (error ("defineSyntaxValueExpand: " ++ show stxs))

defineValueBodyExpand :: [Syntax] -> Expand (Name, StxIdt, Syntax)
defineValueBodyExpand [StxAtom ctx atom, stx] = do
  let idt = StxIdt ctx atom
  name <- introBinding idt
  stx' <- syntaxExpand stx
  pure (name, idt, stx')
defineValueBodyExpand stxs = do
  pure (error (show stxs))

--------------------------------------------------------------------------------

lambdaBodyExpand :: [StxIdt] -> [Syntax] -> Expand (ScopeId, [Syntax])
lambdaBodyExpand args body = do
  scope <- newIntroScopeId

  args' <- traverse (scopeStxIdt scope) args
  forms <- traverse makeArgTransform args'
  body' <- traverse (scopeSyntax scope) body

  local (extends forms) do
    prev'usages <- state \env ->
      (state'usage'scopes env, env {state'usage'scopes = ScopeSet.empty})

    final <- traverse syntaxExpand body'

    modify' (set stwUsageScopes prev'usages)

    pure (scope, final)
  where
    makeArgTransform :: StxIdt -> Expand (Name, Transform)
    makeArgTransform arg = do
      name <- introBinding arg
      pure (name, Transform.Var arg)

--------------------------------------------------------------------------------

letBindExpand :: Syntax -> Expand Syntax
letBindExpand stx = do
  (var, rhs) <- parse (Parse.pStxLetSyntaxBind stx)
  rhs' <- syntaxExpand rhs
  pure (StxList stx.context [var.syntax, rhs'])

letVarsExpand :: Syntax -> Expand Syntax
letVarsExpand (StxAtom ctx atom) = do
  let exn = ExnParseLetBind (StxAtom ctx atom)
  throwError (ExnParseError exn)
letVarsExpand (StxList ctx stxs) = do
  stxs' <- traverse letBindExpand stxs
  pure (StxList ctx stxs')

--------------------------------------------------------------------------------

letSyntaxExpand :: [(StxIdt, Syntax)] -> Syntax -> Expand Syntax
letSyntaxExpand vars body = do
  scope <- newIntroScopeId

  names <- traverse (makeLetSyntaxBind scope) vars
  forms <- traverse makeDatumTransform names
  body' <- scopeSyntax scope body

  local (extends forms) do
    final <- syntaxExpand body'
    modify' (over stwIntroScopes (ScopeSet.insert scope))
    pure final
  where
    makeLetSyntaxBind :: ScopeId -> (StxIdt, Syntax) -> Expand (Name, Syntax)
    makeLetSyntaxBind sc (idt, stx) = do
      idt' <- scopeStxIdt sc idt
      name <- introBinding idt'
      pure (name, stx)

    makeDatumTransform :: (Name, Syntax) -> Expand (Name, Transform)
    makeDatumTransform (name, stx) = do
      value <- letSyntaxBindExpand stx
      pure (name, Transform.Dtm value)

letSyntaxBindExpand :: Syntax -> Expand Datum
letSyntaxBindExpand stx = do
  prev'prunes <- state \env ->
    (state'intro'scopes env, env {state'intro'scopes = ScopeSet.empty})

  final <- nextPhase do
    stx' <- syntaxExpand stx
    expr <- parse (Parse.pSyntax stx')
    exprEval expr

  modify' (set stwIntroScopes prev'prunes)
  pure final

--------------------------------------------------------------------------------

applyTransformer :: Expr -> Syntax -> Expand Syntax
applyTransformer func stx = do
  intro <- newIntroScopeId
  usage <- newUsageScopeId
  stx' <- flipsSyntax intro =<< scopeSyntax usage stx

  let macro = SExpApp func [SExpVal (Datum.Stx stx')]
  exprEval macro >>= \case
    Datum.Stx result -> do
      stx'e <- flipsSyntax intro result
      syntaxExpand stx'e
    other -> do
      throwError (ExnRecievedNotSyntax other)