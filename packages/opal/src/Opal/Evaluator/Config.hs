{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Evaluator.Config
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
module Opal.Evaluator.Config
  ( -- * EvalConfig
    EvalConfig (..)
    -- ** Lenses
  , evalEnvironment
  , evalCurrentPhase
  , evalCurrentScope
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Binding.Environment (Environment)
import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase)
import Opal.Common.Scope (Scope)

-- EvalConfig ------------------------------------------------------------------

-- | 'EvalConfig' is the read-only state of the 'Eval' monad.
--
-- @since 1.0.0
data EvalConfig = EvalConfig
  { eval_environment   :: Environment
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

$(defineLenses ''EvalConfig)

-- | @since 1.0.0
instance Default EvalConfig where
  def = EvalConfig def def def
