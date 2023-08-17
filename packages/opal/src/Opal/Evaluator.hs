{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Evaluator
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Evaluator
  ( -- * Eval
    Eval (..)
    -- ** Basic Operations
  , runEval
  , runEvalSExp
    -- ** Evaluate
  , evalSExp
  , evalSApp
  , evalSExpBody
    -- ** Query
  , getVariable
  , putVariable
    -- * EvalConfig
  , EvalConfig (..)
    -- * EvalError
  , EvalError (..)
    -- * EvalState
  , EvalState (..)
  )
where

import Control.Lens (set, view)

import Control.Monad (unless)
import Control.Monad.Except (MonadError(..))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader(..))

import Data.Default (Default (..))
import Data.IORef (newIORef)
import Data.IORef qualified as IORef
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty

import Opal.Binding.Environment (Environment)
import Opal.Binding.Environment qualified as Environment
import Opal.Common.Symbol (Symbol (..))
import Opal.Evaluator.Monad
  ( Eval (..)
  , EvalConfig (..)
  , EvalError (..)
  , EvalState (..)
  , evalEnvironment
  , runEval
  )
import Opal.Syntax
  ( Datum (..)
  , Identifier (..)
  , Lambda (..)
  , SExp (..)
  , lambdaArity
  )
import Opal.Syntax.Transformer
import Opal.Error (ErrorNotBound(ErrorNotBound))

-- Eval - Evaluate -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runEvalSExp :: EvalConfig -> EvalState -> SExp -> IO (Either EvalError (Datum, EvalState))
runEvalSExp c s0 = runEval c s0 . evalSExp

-- | TODO: docs
--
-- @since 1.0.0
evalSExp :: SExp -> Eval Datum
evalSExp (SVal val) = pure val
evalSExp (SVar var) = getVariable var
evalSExp (SApp app) = do
  evalSExp (NonEmpty.head app) >>= \case
    DatumLam fun -> evalSApp fun (NonEmpty.tail app)
    _            -> error ("cannot apply arguments to non-function datum: " <> show app)

-- | TODO: docs
--
-- @since 1.0.0
evalSApp :: Lambda -> [SExp] -> Eval Datum
evalSApp lam@(Lambda args body) sexps = do
  let arity = lambdaArity lam
  let count = length sexps

  unless (arity == count) do
    error ("expected " ++ show arity ++ " arguments, got " ++ show count)

  env  <- view evalEnvironment
  env' <- foldr insertEnvironment (pure env) (zip args sexps)

  local (set evalEnvironment env') (evalSExp body)
  where
    insertEnvironment :: (Symbol, SExp) -> Eval Environment -> Eval Environment
    insertEnvironment (arg, sexp) next = do
      val <- evalSExp sexp
      tfm <- liftIO (fmap TfmVal (newIORef val))
      fmap (Environment.insert arg tfm) next

-- | TODO: docs
--
-- @since 1.0.0
evalSExpBody :: NonEmpty SExp -> Eval Datum
evalSExpBody (sexp :| sexps) = do
  val <- evalSExp sexp
  case NonEmpty.nonEmpty sexps of
    Nothing     -> pure val
    Just sexps' -> evalSExpBody sexps'

-- Eval - Query ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
getVariable :: Symbol -> Eval Datum
getVariable var = do
  env <- view evalEnvironment
  case Environment.lookup var env of
    Nothing           -> throwError (EvalNotBound (ErrorNotBound (Identifier var def) var))
    Just (TfmVal ref) -> liftIO (IORef.readIORef ref)
    Just _            -> error ("getVariable: bad syntax: " <> show var)

-- | TODO: docs
--
-- @since 1.0.0
putVariable :: Symbol -> Datum -> Eval ()
putVariable var val = do
  env <- view evalEnvironment
  case Environment.lookup var env of
    Nothing           -> throwError (EvalNotBound (ErrorNotBound (Identifier var def) var))
    Just (TfmVal ref) -> liftIO (IORef.writeIORef ref val)
    Just _            -> error ("putVariable: bad syntax: " <> show var)
