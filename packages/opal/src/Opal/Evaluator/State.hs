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
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Common.Lens (defineLenses)

-- EvalState -------------------------------------------------------------------

-- | 'EvalState' is the mutable state of the 'Eval' monad.
--
-- Note: @eval_intro_scopes@ and @eval_usage_scopes@ were removed as
-- part of the macro-state refactor. They were declared but never
-- read; the expander threaded them in via @expanderEval@ but the
-- evaluator never consumed them. See
-- @plans\/expander-macro-state-refactor.md@.
--
-- @since 1.0.0
data EvalState = EvalState
  { eval_binding_store :: BindingStore
  -- ^ A binding store that is threaded through evaluation and expansion.
  }
  deriving (Eq, Generic, Ord, Show)

$(defineLenses ''EvalState)

-- | @since 1.0.0
instance Default EvalState where
  def = EvalState def