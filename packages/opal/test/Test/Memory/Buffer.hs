{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Memory.Buffer
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Memory.Buffer (testTree) where

import Hedgehog (assert, evalIO, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Opal.Memory.Buffer 
  ( bufferEq
  , newBuffer
  , packBuffer
  , unpackBuffer
  )

import Test.Core (TestTree, testCase, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree = 
  testGroup "buffer"
    [ testCase "bufferEq" do 
        len <- forAll $ Gen.integral $ Range.linear 0 100
        buf <- evalIO $ newBuffer len
        ret <- evalIO $ bufferEq buf buf
        assert ret
    , testCase "packBuffer/unpackBuffer" do 
        xs  <- forAll $ Gen.list (Range.linear 0 100) (Gen.word8 Range.constantBounded)
        buf <- evalIO $ packBuffer xs
        ys  <- evalIO $ unpackBuffer buf
        xs === ys
    ]