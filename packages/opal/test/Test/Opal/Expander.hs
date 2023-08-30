{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Expander
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Expander
  ( testTree
  )
where

import Test.Core (TestTree, testCase, testGroup, testUnit)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "expander"
    [
    ]