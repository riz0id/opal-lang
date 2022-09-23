{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand
  ( -- * TODO
    ExpM (ExpM, unExpM),
    evalExpM,

    -- * TODO
    runStxExpand,

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
import Data.Map.Strict qualified as Map
import Data.SrcLoc (SrcLoc (SrcLoc))

import Prelude hiding (exp)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

import Opal.Expr (Datum (LitDtm), Expr)

import Opal.Expand.Context (Context (Context), ctxBindings, ctxIntroScopes, ctxPhase, ctxUseScopes, ctxEnvironment)
import Opal.Expand.Context qualified as Context
import Opal.Expand.Eval (EvalCtx (..), EvalState (EvalState), evalAppStxLocalValue, Eval, evalAppStxExp)
import Opal.Expand.Eval qualified as Eval
import Opal.Expand.Monad (ExpError (..), ExpM (..))
import Opal.Expand.Resolve (evalResolveM, newBind, newScopeId, resolve)
import Opal.Expand.Syntax (ScopeId, StxCtx (StxCtx), StxIdt (StxIdt), Syntax)
import Opal.Expand.Syntax qualified as Syntax
import Opal.Expand.Syntax.MultiScopeSet (Phase (Phase))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Transformer (Transform (..))
import Opal.Expr qualified as Expr
import Opal.AST.Literal (Literal(BoolLit))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalExpM :: ExpM a -> Either ExpError a
evalExpM (ExpM k) =
  let context :: Context
      context = Context (Phase 0) mempty mempty mempty mempty
   in evalResolveM (k context)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runStxExpand :: Syntax -> ExpM Expr
runStxExpand stx = do
  phase <- asks Context.ctxPhase
  coreScope <- newScopeId
  introCoreBind "lambda" coreScope LamTfm do
    introCoreBind "syntax" coreScope StxTfm do
      introCoreBind "quote" coreScope QteTfm do
        introCoreBind "let-syntax" coreScope LetTfm do
          introCoreBind "#t" coreScope (DtmTfm $ LitDtm $ BoolLit True) do
            introCoreBind "syntax-e" coreScope (VarTfm "syntax-e") do
              introCoreBind "syntax-local-value" coreScope (VarTfm "syntax-local-value") do
                expStx (Syntax.scope phase coreScope stx)

introCoreBind :: String -> ScopeId -> Transform -> ExpM a -> ExpM a
introCoreBind name scopes tfm emx = do
  let sym = Symbol.pack name
  let ctx = StxCtx (SrcLoc 0 0 0) (MultiScopeSet.singleton (Phase 0) scopes)
  binding <- newBind (Phase 0) (StxIdt ctx sym)
  local (Context.newTransformer binding.symbol tfm) emx

--------------------------------------------------------------------------------

withVariableBinding :: StxIdt -> ExpM a -> ExpM a
withVariableBinding idt ex = do
  phase <- asks Context.ctxPhase
  binding <- newBind phase idt
  local (Context.newTransformer binding.symbol $ VarTfm idt.symbol) ex

withDatumBinding :: StxIdt -> Datum -> ExpM a -> ExpM a
withDatumBinding idt val ex = do
  phase <- asks Context.ctxPhase
  binding <- newBind phase idt
  local (Context.newTransformer idt.symbol $ DtmTfm val) do
    local (Context.newTransformer binding.symbol $ DtmTfm val) ex

resolveSyntax :: StxIdt -> ExpM Symbol
resolveSyntax idt = do
  phase <- asks Context.ctxPhase
  resolve phase idt >>= \case
    Nothing -> error ("ambiguous bindings to identifier: " ++ show idt)
    Just symbol -> pure symbol

resolveTransform :: StxIdt -> ExpM Transform
resolveTransform idt = do
  symbol <- resolveSyntax idt
  expEnv <- asks Context.ctxEnvironment
  case Map.lookup symbol expEnv of
    Nothing -> error ("generated symbol '" ++ show idt ++ "' not bound to any transformer (impossible?)")
    Just transformer -> pure transformer

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
expStx :: Syntax -> ExpM Expr
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
expApp :: [Syntax] -> StxCtx -> ExpM Expr
expApp stxs ctxApp = case stxs of
  [] -> error ("empty syntax application @ " ++ show ctxApp)
  Syntax.Lit {} : _ -> do
    throwError (LitStxAppError stxs)
  Syntax.Idt idt : stxArgs -> do
    expAppIdt idt stxArgs ctxApp
  Syntax.App {} : _ -> do
    exprs <- traverse expStx stxs
    pure (Expr.AppExp exprs)

makeStxExp :: Syntax -> Expr
makeStxExp = Expr.DtmExp . Expr.StxDtm

makeFunExp :: Symbol -> Expr -> Expr
makeFunExp var body = Expr.DtmExp (Expr.FunDtm var body)

expAppIdt :: StxIdt -> [Syntax] -> StxCtx -> ExpM Expr
expAppIdt idtFun stxArgs ctxApp =
  resolveTransform idtFun >>= \case
    QteTfm -> do
      if length stxArgs == 1
        then pure (makeStxExp $ head stxArgs)
        else error ("application to 'quote': expected 1 argument, got " ++ shows (length stxArgs) ": " ++ show stxArgs)
    StxTfm ->
      if length stxArgs == 1
        then pure (makeStxExp $ head stxArgs)
        else error ("application to 'syntax': expected 1 argument, got " ++ shows (length stxArgs) ": " ++ show stxArgs)
    LamTfm -> do
      unless (length stxArgs == 2) do
        error ("application to 'lambda': expected 2 argument, got " ++ shows (length stxArgs) ": " ++ show stxArgs)

      idtVar <- case head stxArgs of
        Syntax.Idt idt -> pure idt
        stx -> error ("application to 'lambda': argument 1: expected identifier, got '" ++ shows stx "'")

      let stxBody = stxArgs List.!! 1

      expStxLam idtVar stxBody
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

expStxLam :: StxIdt -> Syntax -> ExpM Expr
expStxLam idtVar stxBody = do
  phase <- asks Context.ctxPhase
  scope <- newScopeId
  withVariableBinding (Syntax.scope phase scope idtVar) do
    expBody <- expStx (Syntax.scope phase scope stxBody)
    pure (makeFunExp idtVar.symbol expBody)

expStxLet :: StxIdt -> Syntax -> Syntax -> ExpM Expr
expStxLet idtVar stxVal stxBody = do
  expVal <- expStx stxVal
  (st, val) <- expEvaluate expVal
  phase <- asks Context.ctxPhase
  scope <- newScopeId
  let idtVar' :: StxIdt
      idtVar' = Syntax.scope phase scope idtVar
   in withDatumBinding idtVar' val do
        expStx (Syntax.scope phase scope stxBody)

expMacApp :: Datum -> Syntax -> ExpM Expr
expMacApp stxMac stxMacApp = do
  phase <- asks Context.ctxPhase
  scUse <- newScopeId
  scIntro <- newScopeId

  let stxMacApp' = Syntax.flipscope phase scIntro (Syntax.scope phase scUse stxMacApp)
  let exprMacApp = Expr.AppExp [Expr.DtmExp stxMac, makeStxExp stxMacApp']
  expEvaluate exprMacApp >>= \case
    (st, Expr.StxDtm stx) -> expStx (Syntax.flipscope phase scIntro stx)
    (st, val) -> error ("recieved value from expander that was not syntax: " ++ show val)

runExpEval :: Eval a -> ExpM (EvalState, a)
runExpEval eval = do
  ctxEval <- getExpanderEvalCtx
  envEval <- getExpanderEvalState
  case Eval.runEval ctxEval envEval eval of
    (_, Left exn) -> error ("eval error: " ++ show exn)
    (st1, Right rx) -> pure (st1, rx)

expEvaluate :: Expr -> ExpM (EvalState, Datum)
expEvaluate expr = runExpEval (Eval.evalExpr expr) 

getExpanderEvalCtx :: ExpM EvalCtx
getExpanderEvalCtx = do
  phase <- asks ctxPhase
  env <- asks ctxEnvironment
  pure (EvalCtx phase Nothing Map.empty env)
{-# INLINE CONLIKE getExpanderEvalCtx #-}

getExpanderEvalState :: ExpM EvalState
getExpanderEvalState =
  EvalState
    <$> asks ctxBindings
    <*> asks ctxUseScopes
    <*> asks ctxIntroScopes
{-# INLINE CONLIKE getExpanderEvalState #-}