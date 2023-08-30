{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax.Value.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax.Value.Gen
  ( -- * Generators
    value
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Syntax.Value (Value (..))

import Test.Opal.Common.Symbol.Gen qualified as Gen

-- Generators ------------------------------------------------------------------

-- | 'Transformer' generator.
value :: MonadGen m => m Value
value =
  Gen.choice
    [ fmap ValueB   Gen.bool
    , fmap ValueC   Gen.unicode
    , fmap ValueS   Gen.symbol
    , fmap ValueF32 (Gen.float (Range.constant 1e-6 1e6))
    , fmap ValueI32 (Gen.int32 Range.constantBounded)
    , pure ValueVoid
    ]
