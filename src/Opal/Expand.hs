{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
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

import Control.Lens (set, over)

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (modify', state, gets)

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Symbol (Symbol)

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
  scopeSyntax,
 )
import Opal.Expand.Evaluate (exprEval)
import Opal.Expand.Monad
import Opal.Expand.Namespace (makeEmptyNamespace)
import Opal.Expand.Resolve (resolveName)
import Opal.Expand.Syntax (StxCtx, StxIdt (StxIdt), Syntax (..))
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform)
import qualified Opal.Expand.Transform as Transform

import Opal.Parse (ParseError (..))
import Opal.Parse qualified as Parse
import qualified Debug.Trace as Debug
import Opal.Expand.Syntax.MultiScopeSet (Phase(Phase))

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
        stx' <- syntaxExpand =<< scopeSyntax coreScope stx
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
        syntaxExpand stx'

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
      let symbol :: Symbol
          symbol = if bool then "#t" else "#f"
       in pure (StxAtom idt.context symbol)
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
    Transform.Core Core.Form.DefineValue -> do
      let define'value = StxAtom idt.context "define-value"
      (var, rhs) <- parse (Parse.pStxDefineValue stxs)
      (sc, rhs') <- defineValueBodyExpand var rhs
      scopeSyntax sc (StxList ctx [define'value, var.syntax, rhs'])
    Transform.Core Core.Form.Quote -> do
      let quote = StxAtom idt.context "quote"
      pure (StxList ctx (quote : stxs))
    Transform.Core Core.Form.Syntax
      | [stx] <- stxs -> do
          phase <- asks ctx'phase 

          if phase /= Phase 0
            then pure (error ("illegal use of syntax at phase " ++ show phase)) 
            else do
              let syntax = StxAtom idt.context "syntax"
              stx' <- pruneSyntax stx
              pure (StxList ctx [syntax, stx'])
      | otherwise -> do
          throwError (ExnParseError (ExnParseSyntax stxs))
    Transform.Core Core.Form.QuasiSyntax
      | [stx] <- stxs -> do
          let syntax = StxAtom idt.context "quasisyntax"
          stx1 <- pruneSyntax stx
          stx2 <- quasiSyntaxExpand stx1
          pure (StxList ctx [syntax, stx2])
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

quasiSyntaxExpand :: Syntax -> Expand Syntax 
quasiSyntaxExpand (StxAtom ctx atom) = do 
  pure (StxAtom ctx atom)
quasiSyntaxExpand (StxList ctx stxs@(StxAtom ctx' atom : stxs')) = do 
  name <- resolveName (StxIdt ctx' atom)
  bind <- indexTransformers name
  case bind of 
    Transform.Core Core.Form.Unsyntax
      | [stx] <- stxs' -> unsyntaxExpand stx
      | otherwise -> error ("unsyntax parse error")
    _ -> do 
      stxs'' <- traverse quasiSyntaxExpand stxs
      pure (StxList ctx stxs'')
quasiSyntaxExpand (StxList ctx stxs) = do 
  stxs' <- traverse quasiSyntaxExpand stxs
  pure (StxList ctx stxs')

unsyntaxExpand :: Syntax -> Expand Syntax 
unsyntaxExpand stx = do 
  prev'prunes <- state \env ->
    (state'intro'scopes env, env {state'intro'scopes = ScopeSet.empty})

  stw <- gets state'bindstore 
  let !_ = Debug.trace (show stw) ()
  
  final <- do
    stx' <- syntaxExpand stx
    expr <- parse (Parse.pSyntax stx')
    exprEval expr

  modify' (set stwIntroScopes prev'prunes)

  case final of 
    Datum.Stx stx'' -> pure stx''
    Datum.Atom atom -> 
      pure (Syntax.makeSyntax stx atom)
    Datum.Bool bool -> do 
      let symbol = if bool then "#t" else "#f"
      pure (Syntax.makeSyntax stx symbol)
    Datum.Prim prim -> do 
      let symbol = Core.Prim.toSymbol prim 
      pure (Syntax.makeSyntax stx symbol)

applicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
applicationExpand ctx func stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx (func.syntax : stxs'))

--------------------------------------------------------------------------------

defineValueBodyExpand :: StxIdt -> Syntax -> Expand (ScopeId, Syntax)
defineValueBodyExpand idt rhs = do
  scope <- newIntroScopeId

  idt' <- scopeStxIdt scope idt
  name <- introBinding idt'
  rhs' <- scopeSyntax scope rhs

  local (extend name (Transform.Var idt')) do
    final'rhs <- syntaxExpand rhs'
    pure (scope, final'rhs)

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

--------------------------------------------------------------------------------

-- bodyExpand :: [Syntax] -> ScopeId -> Expand [Syntax]
-- bodyExpand bodys sc = do 

--   outside'sc <- newScopeId 
--   inside'sc <- newScopeId 

--   init'bodys <- for bodys \body -> 
--     scopeSyntax sc body 
--       >>= scopeSyntax outside'sc
--       >>= scopeSyntax inside'sc

--   body'ctx <- asks \ctx -> 
--     let scopes' = ScopeSet.union [outside'sc, inside'sc] ctx.ctx'scopes

--      in ctx
--           { ctx'scopes = scopes'
--           }

--   undefined