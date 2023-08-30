{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Core.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Core.Gen
  ( -- * Generators
    coreForm
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

import Opal.Core (CoreForm)

-- Generators ------------------------------------------------------------------

-- | 'CoreForm' generator.
coreForm :: MonadGen m => m CoreForm
coreForm = Gen.enumBounded
