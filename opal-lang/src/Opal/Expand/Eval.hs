{-# LANGUAGE OverloadedStrings #-}

module Opal.Expand.Eval
  ( -- * TODO
    Eval (Eval, unEval),

    -- ** TODO
    EvalExn (ExnAppToDatum, ExnUnbound),

    -- ** TODO
    EvalCtx (EvalCtx),

    -- ** TODO
    EvalState (EvalState),

    -- * TODO
    runEval,

    -- * TODO
    evalExpr,
    evalAppStxExp,
    evalAppStxLocalValue,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks, local)

import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Eval.Monad
  ( Eval (Eval, unEval),
    EvalCtx (EvalCtx),
    EvalExn (ExnAppToDatum, ExnUnbound),
    EvalState (EvalState),
    evalEnv,
    indexLocalSyntax,
    valueBind,
  )

import Opal.Expand.Syntax qualified as Syntax
import Opal.Expr
  ( Datum (AtomDtm, FunDtm, ListDtm, StxDtm),
    Expr (AppExp, DtmExp, VarExp),
    stx'exp,
    stx'new,
  )
import Opal.Expand.Transformer
import Opal.Expand.Resolve (MonadResolve(resolve))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runEval :: EvalCtx -> EvalState -> Eval a -> (EvalState, Either EvalExn a)
runEval ctx st0 eval =
  case unEval eval ctx st0 of
    (# st1, (# e | #) #) -> (st1, Left e)
    (# st1, (# | x #) #) -> (st1, Right x)
{-# INLINE runEval #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
evalExpr :: Expr -> Eval Datum
evalExpr (DtmExp val) = pure val
evalExpr (VarExp var) = evalVar var
evalExpr (AppExp exs)
  | null exs = pure (ListDtm [])
  | otherwise = evalAppExpr (head exs) (tail exs)
{-# INLINE evalExpr #-}

-- | TODO
--
-- @since 1.0.0
evalVar :: Symbol -> Eval Datum
evalVar var = do
  result <- asks (Map.lookup var . evalEnv)
  case result of
    Nothing -> throwError (ExnUnbound var)
    Just vx -> pure vx
{-# INLINE evalVar #-}

-- | TODO
--
-- @since 1.0.0
evalAppExpr :: Expr -> [Expr] -> Eval Datum
evalAppExpr (DtmExp val) args = evalAppDatum val args
evalAppExpr (VarExp var) args = do
  result <- asks (Map.lookup var . evalEnv)
  case result of
    Just val -> evalAppDatum val args
    Nothing
      | var == "syntax-e" -> evalAppStxExp args
      | var == "mk-syntax" -> evalAppMkStx args
      | var == "syntax-local-value" -> evalAppStxLocalValue args
      | otherwise -> throwError (ExnUnbound var)
evalAppExpr (AppExp exs) args = do
  fun <- evalExpr (AppExp exs)
  evalAppDatum fun args
{-# INLINE evalAppExpr #-}

-- | TODO
--
-- @since 1.0.0
evalAppDatum :: Datum -> [Expr] -> Eval Datum
evalAppDatum (FunDtm var body) args
  | null args = pure (FunDtm var body)
  | otherwise = do
      val <- evalExpr (head args)
      local (valueBind var val) do
        body' <- evalExpr body
        if null (tail args)
          then pure body'
          else evalAppDatum body' (tail args)
evalAppDatum val args = do
  throwError (ExnAppToDatum val args)
{-# INLINE evalAppDatum #-}

-- | TODO
--
-- @since 1.0.0
evalAppStxExp :: [Expr] -> Eval Datum
evalAppStxExp [DtmExp (StxDtm stx)] = pure (stx'exp stx)
evalAppStxExp args = error ("invalid arguments to 'syntax-e': " ++ show args)

-- | TODO
--
-- @since 1.0.0
evalAppMkStx :: [Expr] -> Eval Datum
evalAppMkStx [DtmExp (AtomDtm atom), DtmExp (StxDtm stx)] = pure (StxDtm (stx'new atom.symbol stx))
evalAppMkStx args = error ("invalid arguments to 'syntax-e': " ++ show args)

-- | TODO
--
-- @since 1.0.0
evalAppStxLocalValue :: [Expr] -> Eval Datum
evalAppStxLocalValue [DtmExp (StxDtm (Syntax.Idt idt))] = do 
  asks (indexLocalSyntax idt.symbol) >>= \case
    Nothing -> error ("syntax-local-value('" ++ show idt ++ "'): is unbound")
    Just tfm -> evalTransform idt.symbol tfm 
evalAppStxLocalValue args =
  error ("invalid arguments to 'syntax-local-value': " ++ show args)

evalTransform :: Symbol -> Transform -> Eval Datum 
evalTransform idt = \case
  LamTfm -> error ("syntax-local-value('" ++ show idt ++ "'): is a 'lambda' transformer")
  StxTfm -> error ("syntax-local-value('" ++ show idt ++ "'): is a 'syntax' transformer")
  QteTfm -> error ("syntax-local-value('" ++ show idt ++ "'): is a 'quote' transformer")
  LetTfm -> error ("syntax-local-value('" ++ show idt ++ "'): is a 'let-syntax' transformer")
  StopTfm tfm -> evalTransform idt tfm
  VarTfm var -> evalVar var
  DtmTfm val -> pure val