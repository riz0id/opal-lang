{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax.Transformer.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax.Transformer.Gen
  ( -- * Generators
    transformer
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

import Opal.Syntax.Transformer (Transformer (..))

import Test.Opal.Core.Gen qualified as Gen
import Test.Opal.Syntax.Gen qualified as Gen

-- Generators ------------------------------------------------------------------

-- | 'Transformer' generator.
transformer :: MonadGen m => m Transformer
transformer =
  Gen.choice
    [ fmap TfmCore  Gen.coreForm
    , fmap TfmDatum Gen.datum
    ]
