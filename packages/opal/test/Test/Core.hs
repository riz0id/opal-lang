{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Core
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Core
  ( -- * Testing
    testCase
  , testUnit

    -- * TestTree
  , TestTree
  , TestName
  , testGroup

    -- * Property
  , PropertyT
  , Property
  ) where

import Hedgehog (PropertyT, Property, property, withTests)
import Hedgehog.Internal.Property (PropertyName (..))

import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.Hedgehog (testPropertyNamed)

--------------------------------------------------------------------------------

-- | TODO: docs
testCase :: TestName -> PropertyT IO () -> TestTree
testCase name = testPropertyNamed name (PropertyName name) . property

-- | Similar to 'testCase', but with only performs a single test trial. This
-- should be used if the test case does not use any generated test values.
testUnit :: TestName -> PropertyT IO () -> TestTree
testUnit name = testPropertyNamed name (PropertyName name) . withTests 1 . property
