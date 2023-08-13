{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Common.Symbol
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Common.Symbol
  ( testTree
  )
where

import Control.Exception (evaluate)

import Hedgehog (evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Common.Symbol
  ( stringToSymbol
  , symbolToString
  , symbolHead
  , symbolTail
  )

import Test.Core (TestTree, testCase, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "symbol"
    [ testCase "stringToSymbol/symbolToString" do
        str1 <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
        sym  <- evalIO $ evaluate $ stringToSymbol str1
        str2 <- evalIO $ evaluate $ symbolToString sym
        str1 === str2
    , testCase "symbolHead" do
        str <- forAll $ Gen.string (Range.linear 1 10) Gen.unicode
        sym <- evalIO $ evaluate $ stringToSymbol str
        head str === symbolHead sym
    , testCase "symbolTail" do
        str <- forAll $ Gen.string (Range.linear 1 10) Gen.unicode
        sym <- evalIO $ evaluate $ stringToSymbol str
        Just (tail str) === fmap symbolToString (symbolTail sym)
    ]