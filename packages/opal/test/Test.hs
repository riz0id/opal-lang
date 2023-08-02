{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Main (main) where

import Test.Tasty (TestTree, defaultMain, testGroup)

import Test.Common.Symbol qualified
import Test.Memory.Buffer qualified
import Test.Reader qualified
import Test.Writer qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree
testTree =
  testGroup "opal"
    [ Test.Common.Symbol.testTree
    , Test.Memory.Buffer.testTree
    , Test.Reader.testTree
    , Test.Writer.testTree
    ]