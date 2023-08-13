{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Common
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Common
  ( testTree
  )
where

import Test.Core (TestTree, testGroup)
import Test.Opal.Common.Symbol qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "common"
    [ Test.Opal.Common.Symbol.testTree
    ]