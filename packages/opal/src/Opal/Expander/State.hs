{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Expander.State
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
module Opal.Expander.State
  ( -- * ExpandState
    ExpandState (..)
    -- ** Basic Operations
  , defaultExpandState
    -- ** Lenses
  , expandBindingStore
  , expandEnvironment
  , expandNamespace
  , expandIntroScopes
  , expandUsageScopes
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Binding.Environment (Environment)
import Opal.Common.Lens (defineLenses)
import Opal.Common.ScopeSet (ScopeSet)
import Opal.Module (Namespace (..))

import Prelude hiding (id)

-- ExpandState -----------------------------------------------------------------

-- | 'ExpandState' is the read-only state of the 'Expand' monad.
--
-- @since 1.0.0
data ExpandState = ExpandState
  { expand_binding_store :: BindingStore
    -- ^ A binding store that is threaded through the expander to substitute
    -- identifiers with the generated symbols they are bound to.
  , expand_environment   :: Environment
    -- ^ The expanders's compile-time environment. This maps bindings to their
    -- compile-time meanings. The 'Symbol' key is a generated symbol obtained
    -- from the expanders's binding store.
  , expand_namespace     :: {-# UNPACK #-} !Namespace
    -- ^ TODO: docs
  , expand_intro_scopes  :: ScopeSet
  -- ^ The set of scopes to be pruned at syntax forms.
  , expand_usage_scopes  :: ScopeSet
  -- ^ A subset of the current expansion context's use-stire scopes.
  }
  deriving (Generic, Show)

$(defineLenses ''ExpandState)

-- | 'ExpandState' defaults to 'defaultExpandState'.
--
-- @since 1.0.0
instance Default ExpandState where
  def = defaultExpandState

-- ExpandState - Basic Operations ----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defaultExpandState :: ExpandState
defaultExpandState = ExpandState def def def def def