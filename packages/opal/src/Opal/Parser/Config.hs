{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Parser.Config
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
module Opal.Parser.Config
  ( -- * ParseConfig
    ParseConfig (..)
    -- ** Lenses
  , parseBindingStore
  , parseCurrentPhase
  )
where

import Data.Default (Default (..))

import GHC.Generics (Generic)

import Opal.Binding.BindingStore (BindingStore)
import Opal.Common.Lens (defineLenses)
import Opal.Common.Phase (Phase)

-- ParseConfig -----------------------------------------------------------------

-- | 'ParseConfig' is the read-only state of the 'Parse' monad.
--
-- @since 1.0.0
data ParseConfig = ParseConfig
  { parse_binding_store :: BindingStore
    -- ^ A binding store that is threaded through parsing to substitute
    -- identifiers with the generated symbols they are bound to.
  , parse_current_phase :: {-# UNPACK #-} !Phase
    -- ^ The current phase that 'Parse' is parsing at.
  }
  deriving (Generic, Show)

$(defineLenses ''ParseConfig)

-- | @since 1.0.0
instance Default ParseConfig where
  def = ParseConfig def def
