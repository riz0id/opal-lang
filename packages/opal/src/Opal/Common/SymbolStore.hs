{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.SymbolStore
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
module Opal.Common.SymbolStore
  ( -- * SymbolStore
    SymbolStore (..),
    --
  )
where

import Data.Default (Default (..))
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap

import Opal.Common.Symbol (Symbol)

-- SymbolStore -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newtype SymbolStore = SymbolStore
  { getSymbolStore :: IntMap Symbol }

-- | 'SymbolStore' defaults to 'empty'.
--
-- @since 1.0.0
instance Default SymbolStore where
  def = empty

-- SymbolStore - Basic Operations ----------------------------------------------

-- | The empty 'SymbolStore'.
--
-- @since 1.0.0
empty :: SymbolStore
empty = SymbolStore IntMap.empty