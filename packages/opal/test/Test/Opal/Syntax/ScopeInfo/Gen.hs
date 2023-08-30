{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax.ScopeInfo.Gen
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax.ScopeInfo.Gen
  ( -- * Generators
    scopeInfo
  ) where

import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen

import Opal.Syntax.Transformer (Transformer (..))

import Test.Opal.Core.Gen qualified as Gen
import Test.Opal.Syntax.Gen qualified as Gen

-- Generators ------------------------------------------------------------------

-- | 'ScopeInfo' generator.
scopeInfo :: MonadGen m => m ScopeInfo
scopeInfo = _
