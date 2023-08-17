{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Evaluator.State
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
module Opal.Evaluator.State
  ( -- * EvalState
    EvalState (..)
    -- ** Lenses
  , evalBindingStore
  , evalIntroScopes
  , evalUsageScopes
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Common.Lens (defineLenses)
import Opal.Common.ScopeSet (ScopeSet)

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

$(defineLenses ''EvalState)

-- | @since 1.0.0
instance Default EvalState where
  def = EvalState def def def