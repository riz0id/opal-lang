{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    Eval (..),
    -- ** Basic Operations
    runEval,
    -- * EvalConfig
    EvalConfig (..),
    -- ** Lenses
    evalEnvironment,
    evalCurrentPhase,
    evalCurrentScope,
    -- * EvalState
    EvalState (..),
    -- ** Lenses
    evalBindingStore,
    evalIntroScopes,
    evalUsageScopes,
  )
where

import Control.Lens (Lens', lens)

import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State (MonadState (..), StateT (..))

import Data.Default (Default (..))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Function ((&))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Common.Phase (Phase)
import Opal.Common.Scope (MonadScope (..), Scope)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.Symbol (MonadGenSym (..), Symbol)
import Opal.Syntax.Transformer (Transformer)

-- Eval ------------------------------------------------------------------------

-- | 'Eval' captures computations that evaluate s-expressions within the Opal
-- expander.
--
-- @since 1.0.0
newtype Eval a = Eval
  { unEval :: ReaderT EvalConfig (StateT EvalState IO) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
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
runEval :: EvalConfig -> EvalState -> Eval a -> IO (a, EvalState)
runEval c s eval =
  unEval eval
    & flip runReaderT c
    & flip runStateT s

-- EvalConfig ------------------------------------------------------------------

-- | 'EvalConfig' is the read-only state of the 'Eval' monad.
--
-- @since 1.0.0
data EvalConfig = EvalConfig
  { eval_environment   :: HashMap Symbol Transformer
    -- ^ The evaluator's compile-time environment. This maps bindings to their
    -- compile-time meanings. The 'Symbol' key is a generated symbol obtained
    -- from the evaluator's binding store.
  , eval_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current evaluator phase for syntax forms.
  , eval_current_scope :: Maybe Scope
    -- ^ An optional introduction scope. When given, this scope will be used for
    -- local expansion.
  }
  deriving (Generic, Show)

-- | @since 1.0.0
instance Default EvalConfig where
  def = EvalConfig HashMap.empty def def

-- EvalConfig - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'eval_environment' field of 'EvalConfig'.
--
-- @since 1.0.0
evalEnvironment :: Lens' EvalConfig (HashMap Symbol Transformer)
evalEnvironment = lens eval_environment \s x -> s { eval_environment = x }

-- | Lens focusing on the 'eval_current_phase' field of 'EvalConfig'.
--
-- @since 1.0.0
evalCurrentPhase :: Lens' EvalConfig Phase
evalCurrentPhase = lens eval_current_phase \s x -> s { eval_current_phase = x }

-- | Lens focusing on the 'eval_usage_scopes' field of 'EvalConfig'.
--
-- @since 1.0.0
evalCurrentScope :: Lens' EvalConfig (Maybe Scope)
evalCurrentScope = lens eval_current_scope \s x -> s { eval_current_scope = x }

-- EvalState -------------------------------------------------------------------

-- | 'EvalState' is the mutable state of the 'Eval' monad.
--
-- @since 1.0.0
data EvalState = EvalState
  { eval_binding_store :: BindingStore
  -- ^ A binding store that is threaded through evaluation and expansion.
  , eval_intro_scopes  :: ScopeSet
  -- ^ The set of scopes to be pruned at syntax forms.
  , eval_usage_scopes  :: ScopeSet
  -- ^ A subset of the current expansion context's use-stire scopes.
  }
  deriving (Eq, Generic, Ord, Show)

-- | @since 1.0.0
instance Default EvalState where
  def = EvalState def def def

-- EvalState - Lenses ----------------------------------------------------------

-- | Lens focusing on the 'eval_binding_store' field of 'EvalState'.
--
-- @since 1.0.0
evalBindingStore :: Lens' EvalState BindingStore
evalBindingStore = lens eval_binding_store \s x -> s { eval_binding_store = x }

-- | Lens focusing on the 'eval_intro_scopes' field of 'EvalState'.
--
-- @since 1.0.0
evalIntroScopes :: Lens' EvalState ScopeSet
evalIntroScopes = lens eval_intro_scopes \s x -> s { eval_intro_scopes = x }

-- | Lens focusing on the 'eval_usage_scopes' field of 'EvalState'.
--
-- @since 1.0.0
evalUsageScopes :: Lens' EvalState ScopeSet
evalUsageScopes = lens eval_usage_scopes \s x -> s { eval_usage_scopes = x }