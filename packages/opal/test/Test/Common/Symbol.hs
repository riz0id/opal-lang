{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Common.Symbol
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Common.Symbol (testTree) where

import Control.Exception (evaluate)

import Hedgehog (evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Common.Symbol 
  ( stringToSymbol 
  , symbolToString
  )

import Test.Core (TestTree, testCase, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree = 
  testGroup "symbol"
    [ testCase "stringToSymbol/symbolToString" do 
        xs  <- forAll $ Gen.string (Range.linear 0 100) Gen.unicode
        buf <- evalIO $ evaluate $ stringToSymbol xs
        ys  <- evalIO $ evaluate $ symbolToString buf
        xs === ys
    ]