{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand
  ( -- * TODO
    evalExpand,
    runExpand,
    Expand (Expand, unExpand),

    -- * TODO
    expandSyntax,

    -- * TODO
    expStx,
    expApp,
    expEvaluate,
  )
where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)

import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Map.Strict qualified as Map

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expr (Datum, Expr)

import Data.Traversable (for)
import Opal.Expand.Context
  ( ctxEnvironment,
    ctxIntroScopes,
    ctxPhase,
    ctxUseScopes,
  )
import Opal.Expand.Context qualified as Context
import Opal.Expand.Eval
  ( Eval,
    EvalCtx (..),
    EvalState (EvalState),
    evalAppStxExp,
    evalAppStxLocalValue,
  )
import Opal.Expand.Eval qualified as Eval
import Opal.Expand.Monad
  ( ExpError (..),
    Expand (Expand, unExpand),
    evalExpand,
    runExpand,
  )
import Opal.Expand.Parse (arity1, pLambda, pSyntax, runParseM)
import Opal.Expand.Resolve.Class (newBind, newScopeId, resolve)
import Opal.Expand.Resolve.Monad (ResolveM (R))
import Opal.Expand.Syntax (ScopeId, StxCtx, StxIdt (StxIdt), Syntax)
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Transformer (Transform (..))
import Opal.Expr qualified as Expr

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
expandExpr :: Expr -> Expand Expr
expandExpr (Expr.VarExp var) = pure (Expr.VarExp var)
expandExpr (Expr.DtmExp val) = expandDatum val
expandExpr (Expr.AppExp exs) = fmap Expr.AppExp (traverse expandExpr exs)

-- | TODO
--
-- @since 1.0.0
expandDatum :: Datum -> Expand Expr
expandDatum (Expr.LitDtm val) =
  pure (Expr.DtmExp $ Expr.LitDtm val)
expandDatum (Expr.StxDtm stx) = do
  expandSyntax stx
expandDatum (Expr.FunDtm var body) = do
  body' <- expandExpr body
  pure (Expr.DtmExp $ Expr.FunDtm var body')
expandDatum (Expr.ListDtm vals) = do
  undefined

-- vals' <- traverse expandDatum vals
-- pure (Expr.AppExp vals')

-- | TODO
--
-- @since 1.0.0
expandSyntax :: Syntax -> Expand Expr
expandSyntax stx = runParseM stx pSyntax >>= expandExpr

--------------------------------------------------------------------------------

withDatumBinding :: StxIdt -> Datum -> Expand a -> Expand a
withDatumBinding idt val ex = do
  phase <- asks Context.ctxPhase
  binding <- newBind phase idt
  local (Context.newTransformer idt.symbol $ DtmTfm val) do
    local (Context.newTransformer binding.symbol $ DtmTfm val) ex

resolveSyntax :: StxIdt -> Expand Symbol
resolveSyntax idt = do
  phase <- asks Context.ctxPhase
  resolve phase idt >>= \case
    Nothing -> error ("ambiguous bindings to identifier: " ++ show idt)
    Just symbol -> pure symbol

resolveTransform :: StxIdt -> Expand Transform
resolveTransform idt = do
  -- symbol <- resolveSyntax idt
  expEnv <- asks Context.ctxEnvironment
  case Map.lookup idt.symbol expEnv of
    Nothing -> error ("unbound identifier: " ++ show idt)
    Just transformer -> pure transformer

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
expStx :: Syntax -> Expand Expr
expStx (Syntax.Lit _ lit) = do
  pure (Expr.DtmExp $ Expr.LitDtm lit)
expStx (Syntax.Idt idt) = do
  resolveTransform idt >>= \case
    QteTfm -> error ("transformer 'quote' used as variable reference: " ++ show idt)
    StxTfm -> error ("transformer 'syntax' used as variable reference: " ++ show idt)
    LamTfm -> error ("transformer 'syntax' used as variable reference: " ++ show idt)
    LetTfm -> error ("transformer 'syntax' used as variable reference: " ++ show idt)
    StopTfm tfm -> undefined
    VarTfm var -> do
      pure (Expr.VarExp var)
    DtmTfm val -> case val of
      Expr.StxDtm {} -> pure (Expr.DtmExp val)
      _ -> error (shows idt ": recieved value from syntax expand that was not syntax: " ++ show val)
expStx (Syntax.App ctxApp stxs)
  | null stxs = error "empty syntax application"
  | otherwise = expApp stxs ctxApp

-- | TODO
--
-- @since 1.0.0
expApp :: [Syntax] -> StxCtx -> Expand Expr
expApp stxs ctxApp =
  case NonEmpty.nonEmpty stxs of
    Nothing ->
      error ("empty syntax application @ " ++ show ctxApp)
    Just (Syntax.Lit {} :| _) -> do
      throwError (LitStxAppError stxs)
    Just (Syntax.Idt idt :| stxs') -> do
      expAppIdt idt stxs' ctxApp
    Just (stx@Syntax.App {} :| stxs') -> do
      exprs <- traverse expStx (stx :| stxs')
      pure (Expr.AppExp exprs)

makeStxExp :: Syntax -> Expr
makeStxExp = Expr.DtmExp . Expr.StxDtm

makeFunExp :: [Symbol] -> Expr -> Expr
makeFunExp vars body = Expr.DtmExp (Expr.FunDtm vars body)

expAppIdt :: StxIdt -> [Syntax] -> StxCtx -> Expand Expr
expAppIdt idtFun stxArgs ctxApp =
  resolveTransform idtFun >>= \case
    QteTfm -> do
      stx <- runParseM (Syntax.App ctxApp stxArgs) arity1
      pure (Expr.DtmExp $ Expr.stx'datum stx)
    StxTfm -> do
      stx <- runParseM (Syntax.App ctxApp stxArgs) arity1
      pure (makeStxExp stx)
    LamTfm -> do
      (vars, body) <- runParseM (Syntax.App ctxApp stxArgs) pLambda
      expStxLam vars body
    LetTfm -> do
      unless (length stxArgs == 2) do
        error ("application to 'let-syntax': expected 2 argument, got " ++ shows (length stxArgs) ": " ++ show stxArgs)

      (idtVar, stxVal) <- case head stxArgs of
        Syntax.App _ [Syntax.Idt idtVar, stxVal] -> pure (idtVar, stxVal)
        stx -> error ("application to 'let-syntax': argument 1: expected variable binding, got '" ++ shows stx "'")

      let stxBody = stxArgs List.!! 1

      expStxLet idtVar stxVal stxBody
    StopTfm tfm -> undefined
    VarTfm var
      | var == "syntax-e" -> do
          valArgs <- traverse expStx stxArgs
          (_, val) <- runExpEval (evalAppStxExp valArgs)
          pure (Expr.DtmExp val)
      | var == "syntax-local-value" -> do
          valArgs <- traverse expStx stxArgs
          (_, val) <- runExpEval (evalAppStxLocalValue valArgs)
          pure (Expr.DtmExp val)
      | otherwise -> do
          binding <- resolveSyntax (StxIdt idtFun.context var)
          expAppIdt (StxIdt idtFun.context binding) stxArgs ctxApp
    DtmTfm val -> do
      let stxMacApp :: Syntax
          stxMacApp = Syntax.App ctxApp (idtFun.syntax : stxArgs)
       in expMacApp val stxMacApp

expStxLam :: [StxIdt] -> Syntax -> Expand Expr
expStxLam vars stxBody = do
  foldr withLambdaVar expandLambdaBody vars []
  where
    expandLambdaBody :: [ScopeId] -> Expand Expr
    expandLambdaBody scopes = do
      phase <- asks Context.ctxPhase
      body' <- expStx (foldr ((.) . Syntax.scope phase) id scopes stxBody)
      pure (makeFunExp (map (\var -> var.symbol) vars) body')

    withLambdaVar ::
      StxIdt ->
      ([ScopeId] -> Expand a) ->
      ([ScopeId] -> Expand a)
    withLambdaVar var ex scopes = do
      phase <- asks Context.ctxPhase
      scope <- newScopeId
      binding <- newBind phase var
      local (Context.newTransformer var.symbol $ VarTfm binding.symbol) do
        local (Context.newTransformer binding.symbol $ VarTfm var.symbol) do
          ex (scope : scopes)

expStxLet :: StxIdt -> Syntax -> Syntax -> Expand Expr
expStxLet idtVar stxVal stxBody = do
  expVal <- expStx stxVal
  (st, val) <- expEvaluate expVal
  phase <- asks Context.ctxPhase
  scope <- newScopeId
  let idtVar' :: StxIdt
      idtVar' = Syntax.scope phase scope idtVar
   in withDatumBinding idtVar' val do
        expStx (Syntax.scope phase scope stxBody)

expMacApp :: Datum -> Syntax -> Expand Expr
expMacApp stxMac stxMacApp = do
  phase <- asks Context.ctxPhase
  scUse <- newScopeId
  scIntro <- newScopeId

  let stxMacApp' = Syntax.flipscope phase scIntro (Syntax.scope phase scUse stxMacApp)
  let exprMacApp = Expr.AppExp (Expr.DtmExp stxMac :| [makeStxExp stxMacApp'])
  expEvaluate exprMacApp >>= \case
    (st, Expr.StxDtm stx) -> expStx (Syntax.flipscope phase scIntro stx)
    (st, val) -> error ("recieved value from expander that was not syntax: " ++ show val)

runExpEval :: Eval a -> Expand (EvalState, a)
runExpEval eval = do
  ctxEval <- getExpanderEvalCtx
  envEval <- getExpanderEvalState
  case Eval.runEval ctxEval envEval eval of
    (_, Left exn) -> error ("eval error: " ++ show exn)
    (st1, Right rx) -> pure (st1, rx)

expEvaluate :: Expr -> Expand (EvalState, Datum)
expEvaluate expr = runExpEval (Eval.evalExpr expr)

getExpanderEvalCtx :: Expand EvalCtx
getExpanderEvalCtx = do
  phase <- asks ctxPhase
  env <- asks ctxEnvironment
  pure (EvalCtx phase Nothing Map.empty env)
{-# INLINE CONLIKE getExpanderEvalCtx #-}

getExpanderEvalState :: Expand EvalState
getExpanderEvalState = do
  binds <- Expand \_ ->
    R \binds id# sc# ->
      (# binds, id#, sc#, Right binds #)
  EvalState binds
    <$> asks ctxUseScopes
    <*> asks ctxIntroScopes
{-# INLINE CONLIKE getExpanderEvalState #-}