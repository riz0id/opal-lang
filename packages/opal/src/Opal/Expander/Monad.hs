{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.Monad
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'Expand' monad along with its associated read-only
-- state 'ExpandConfig'.
--
-- @since 1.0.0
module Opal.Expander.Monad
  ( -- * Expand
    Expand (..)
    -- ** Basic Operations
  , runExpand
    -- * ExpandConfig
  , ExpandConfig (..)
    -- ** Lenses
  , expandEnvironment
  , expandCurrentPhase
  , expandContext
    -- * ExpandError
  , ExpandError (..)
    -- * ExpandState
  , ExpandState (..)
    -- ** Lenses
  , expandBindingStore
  , expandIntroScopes
  , expandUsageScopes
    -- * ExpansionContext
  , ExpansionContext (..)
  )
where

import Control.Lens (Lens', lens)

import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import Control.Monad.State.Strict (MonadState (..), StateT(..))

import Data.Default (Default (..))
import Data.Function ((&))
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap

import GHC.Generics (Generic)

import Opal.Common.BindingStore (BindingStore)
import Opal.Common.Phase (Phase)
import Opal.Common.Scope (MonadScope (..))
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Common.Symbol (MonadGenSym (..), Symbol)
import Opal.Syntax

-- Expand -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype Expand a = Expand
  { unExpand :: ReaderT ExpandConfig (StateT ExpandState (ExceptT ExpandError IO)) a }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadError ExpandError
    , MonadReader ExpandConfig
    , MonadState ExpandState
    )

-- | @since 1.0.0
instance MonadGenSym Expand where
  newGenSym = liftIO newGenSym

-- | @since 1.0.0
instance MonadScope Expand where
  newScope = liftIO newScope

-- Expand - Basic Operations ----------------------------------------------------

-- | Run an 'Expand' computation with the given 'ExpandConfig' and initial
-- 'ExpandState'.
--
-- @since 1.0.0
runExpand ::
  ExpandConfig ->
  ExpandState ->
  Expand a ->
  IO (Either ExpandError (a, ExpandState))
runExpand c s0 expand =
  unExpand expand
    & flip runReaderT c
    & flip runStateT s0
    & runExceptT

-- ExpandConfig -----------------------------------------------------------------

-- | 'ExpandConfig' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandConfig = ExpandConfig
  { expand_environment   :: HashMap Symbol Transformer
    -- ^ The expanders's compile-time environment. This maps bindings to their
    -- compile-time meanings. The 'Symbol' key is a generated symbol obtained
    -- from the expanders's binding store.
  , expand_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current expansion phase.
  , expand_context       :: ExpansionContext
    -- ^ The current expansion context.
  }
  deriving (Generic, Show)

-- | @since 1.0.0
instance Default ExpandConfig where
  def = ExpandConfig HashMap.empty def def

-- ExpandConfig - Lenses -------------------------------------------------------

-- | Lens focusing on the 'expand_environment' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandEnvironment :: Lens' ExpandConfig (HashMap Symbol Transformer)
expandEnvironment = lens expand_environment \s x -> s { expand_environment = x }

-- | Lens focusing on the 'expand_current_phase' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandCurrentPhase :: Lens' ExpandConfig Phase
expandCurrentPhase = lens expand_current_phase \s x -> s { expand_current_phase = x }

-- | Lens focusing on the 'expand_context' field of 'ExpandConfig'.
--
-- @since 1.0.0
expandContext :: Lens' ExpandConfig ExpansionContext
expandContext = lens expand_context \s x -> s { expand_context = x }

-- ExpandError -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExpandError
  = ErrorNotInScope {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  | ErrorNotBound {-# UNPACK #-} !Identifier {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  | ErrorBadSyntax {-# UNPACK #-} !Syntax
    -- ^ TODO: docs
  deriving (Show)

-- ExpandState -----------------------------------------------------------------

-- | 'ExpandState' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandState = ExpandState
  { expand_binding_store :: BindingStore
    -- ^ A binding store that is threaded through the expander to substitute
    -- identifiers with the generated symbols they are bound to.
  , expand_intro_scopes  :: ScopeSet
  -- ^ The set of scopes to be pruned at syntax forms.
  , expand_usage_scopes  :: ScopeSet
  -- ^ A subset of the current expansion context's use-stire scopes.
  }
  deriving (Generic, Show)

-- | @since 1.0.0
instance Default ExpandState where
  def = ExpandState def def def

-- ExpandState - Lenses --------------------------------------------------------

-- | Lens focusing on the 'expand_binding_store' field of 'ExpandState'.
--
-- @since 1.0.0
expandBindingStore :: Lens' ExpandState BindingStore
expandBindingStore = lens expand_binding_store \s x -> s { expand_binding_store = x }

-- | Lens focusing on the 'expand_intro_scopes' field of 'ExpandState'.
--
-- @since 1.0.0
expandIntroScopes :: Lens' ExpandState ScopeSet
expandIntroScopes = lens expand_intro_scopes \s x -> s { expand_intro_scopes = x }

-- | Lens focusing on the 'expand_usage_scopes' field of 'ExpandState'.
--
-- @since 1.0.0
expandUsageScopes :: Lens' ExpandState ScopeSet
expandUsageScopes = lens expand_usage_scopes \s x -> s { expand_usage_scopes = x }

-- ExpansionContext ------------------------------------------------------------

-- | 'ExpansionContext' is an enumeration of the various expansion contexts.
--
-- @since 1.0.0
data ExpansionContext
  = ContextExpression
    -- ^ TODO: docs
  | ContextTopLevel
    -- ^ TODO: docs
  | ContextModule
    -- ^ TODO: docs
  | ContextModuleBegin
    -- ^ TODO: docs
  | ContextDefinition
    -- ^ TODO: docs
  deriving (Enum, Eq, Ord)

-- | @since 1.0.0
instance Default ExpansionContext where
  def = ContextTopLevel

-- | @since 1.0.0
instance Show ExpansionContext where
  show ContextExpression   = "expression"
  show ContextTopLevel     = "top-level"
  show ContextModule       = "module"
  show ContextModuleBegin  = "module-begin"
  show ContextDefinition   = "definition"