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
import Control.Monad.State (gets, modify', state)

import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty)
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
  scopeSyntax,
 )
import Opal.Expand.Evaluate (exprEval)
import Opal.Expand.Monad
import Opal.Expand.Resolve.Class (resolveName)
import Opal.Expand.Syntax (
  StxCtx,
  StxIdt (StxIdt),
  Syntax (StxAtom, StxBool, StxList, StxPair, StxVoid),
 )
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet
import Opal.Expand.Transform (Transform)
import Opal.Expand.Transform qualified as Transform
import Opal.Expand.Syntax.StxCtx qualified as StxCtx

import Opal.Parse (ParseError (..))
import Opal.Parse qualified as Parse

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runSyntaxEval :: Syntax -> Either ExpandError Datum
runSyntaxEval stx =
  let stw = makeExpandStore
   in runExpand stw do
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
  let stw = makeExpandStore
   in runExpand stw do
        let coreScope = ScopeId 0
        introCoreBinds
        stx' <- scopeSyntax coreScope stx
        syntaxExpand stx'

-- | TODO
--
-- @since 1.0.0
runExpand :: ExpandStore -> Expand a -> Either ExpandError a
runExpand st0 mx =
  case unExpand mx st0 of
    (# _, (# e | #) #) -> Left e
    (# _, (# | x #) #) -> Right x
{-# INLINE runExpand #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
syntaxExpand :: Syntax -> Expand Syntax
syntaxExpand (StxVoid ctx) = do
  pure (StxVoid ctx)
syntaxExpand (StxBool ctx bool) = do
  pure (StxBool ctx bool)
syntaxExpand (StxPair ctx stx0 stx1) = do
  stx0' <- syntaxExpand stx0
  stx1' <- syntaxExpand stx1
  pure (StxPair ctx stx0' stx1')
syntaxExpand (StxAtom ctx atom) = do
  stxIdtExpand (StxIdt ctx atom)
syntaxExpand (StxList ctx stxs) = do
  stxListExpand ctx stxs

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
    Transform.Core Core.Form.Lambda -> do
      (args, body) <- parse (Parse.pLambdaStx (StxList ctx (idt.syntax : stxs)))
      (sc, body') <- lambdaBodyExpand args body

      let args'stx = StxList StxCtx.empty (map (\arg -> arg.syntax) args)
      let body'stx = toList body'
      scopeSyntax sc (StxList ctx (idt.syntax : args'stx : body'stx))
    Transform.Core Core.Form.If
      | [stx1, stx2, stx3] <- stxs -> do
          let if' = StxAtom idt.context "if"
          stx1' <- syntaxExpand stx1
          stx2' <- syntaxExpand stx2
          stx3' <- syntaxExpand stx3
          pure (StxList ctx [if', stx1', stx2', stx3'])
      | otherwise -> do
          throwError (ExnParseError (ExnParseIf idt.syntax stxs))
    Transform.Core Core.Form.Let -> do
      (vars, body) <- parse (Parse.pLetStx (StxList ctx (idt.syntax : stxs)))
      vars' <- traverse syntaxExpand vars
      (sc, body') <- lambdaBodyExpand (Map.keys vars) body

      let vars'stx = StxList StxCtx.empty (Map.foldrWithKey' (\var rhs rest -> StxList StxCtx.empty [var.syntax, rhs] : rest) [] vars')
      let body'stx = toList body'
      scopeSyntax sc (StxList ctx (idt.syntax : vars'stx : body'stx))
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

applicationExpand :: StxCtx -> StxIdt -> [Syntax] -> Expand Syntax
applicationExpand ctx func stxs = do
  stxs' <- traverse syntaxExpand stxs
  pure (StxList ctx (func.syntax : stxs'))

--------------------------------------------------------------------------------

lambdaBodyExpand ::
  [StxIdt] ->
  NonEmpty Syntax ->
  Expand (ScopeId, NonEmpty Syntax)
lambdaBodyExpand args body = do
  scope <- newIntroScopeId

  args' <- traverse (scopeStxIdt scope) args
  forms <- traverse makeArgTransform args'
  body' <- traverse (scopeSyntax scope) body

  old <- gets (view stwEnvironment)
  modify' (over stwEnvironment \env -> foldr (uncurry Map.insert) env forms)
  prev'usages <- state \env ->
    (state'usage'scopes env, env {state'usage'scopes = ScopeSet.empty})

  final <- traverse syntaxExpand body'

  modify' (set stwUsageScopes prev'usages)
  modify' (set stwEnvironment old)

  pure (scope, final)
  where
    makeArgTransform :: StxIdt -> Expand (Name, Transform)
    makeArgTransform arg = do
      name <- introBinding arg
      pure (name, Transform.Var arg)

--------------------------------------------------------------------------------

letSyntaxExpand :: [(StxIdt, Syntax)] -> Syntax -> Expand Syntax
letSyntaxExpand vars body = do
  scope <- newIntroScopeId

  names <- traverse (makeLetSyntaxBind scope) vars
  forms <- traverse makeDatumTransform names
  body' <- scopeSyntax scope body

  old <- gets (view stwEnvironment)
  modify' (over stwEnvironment \env -> foldr (uncurry Map.insert) env forms)

  final <- syntaxExpand body'

  modify' (over stwIntroScopes (ScopeSet.insert scope))
  modify' (set stwEnvironment old)

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