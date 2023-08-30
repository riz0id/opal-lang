{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Common.Symbol.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Common.Symbol.Gen
  ( -- * Generators
    symbol
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Common.Symbol (Symbol (..), stringToSymbol)

-- Generators ------------------------------------------------------------------

-- | 'Symbol' generator.
symbol :: MonadGen m => m Symbol
symbol = fmap stringToSymbol (Gen.string (Range.linear 1 64) Gen.alphaNum)