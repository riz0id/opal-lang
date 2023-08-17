{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Evaluator.Monad
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Eval' monad along with its associated read-only
-- and mutable state.
--
-- @since 1.0.0
module Opal.Evaluator.Monad
  ( -- * Eval
    Eval (..)
    -- ** Basic Operations
  , runEval
    -- * EvalConfig
  , EvalConfig (..)
    -- ** Lenses
  , evalEnvironment
  , evalCurrentPhase
  , evalCurrentScope
    -- * EvalError
  , EvalError (..)
    -- * EvalState
  , EvalState (..)
    -- ** Lenses
  , evalBindingStore
  , evalIntroScopes
  , evalUsageScopes
  )
where

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..))

import Data.Function ((&))

import Opal.Common.Scope (MonadScope (..))
import Opal.Common.Symbol (MonadGenSym (..))
import Opal.Evaluator.Config
  ( EvalConfig(..)
  , evalEnvironment
  , evalCurrentPhase
  , evalCurrentScope
  )
import Opal.Evaluator.State
  ( EvalState(..)
  , evalBindingStore
  , evalIntroScopes
  , evalUsageScopes
  )
import Opal.Error (ErrorNotBound, Error (..))
import Opal.Writer (Display (..))

-- Eval ------------------------------------------------------------------------

-- | 'Eval' captures computations that evaluate s-expressions within the Opal
-- expander.
--
-- @since 1.0.0
newtype Eval a = Eval
  { unEval :: ReaderT EvalConfig (StateT EvalState (ExceptT EvalError IO)) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError EvalError
    , MonadReader EvalConfig
    , MonadState EvalState
    )

instance MonadGenSym Eval where
  newGenSym = liftIO newGenSym

instance MonadScope Eval where
  newScope = liftIO newScope

-- Eval - Basic Operations -----------------------------------------------------

-- | Run an 'Eval' computation with the given 'EvalConfig' and 'EvalState'.
--
-- @since 1.0.0
runEval :: EvalConfig -> EvalState -> Eval a -> IO (Either EvalError (a, EvalState))
runEval c s eval =
  unEval eval
    & flip runReaderT c
    & flip runStateT s
    & runExceptT

-- EvalError -------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data EvalError
  = EvalNotBound {-# UNPACK #-} !ErrorNotBound
    -- ^ TODO: docs

-- | @since 1.0.0
instance Error EvalError where
  errorCode (EvalNotBound exn) = errorCode exn

-- | @since 1.0.0
instance Display EvalError where
  display (EvalNotBound exn) = display exn
