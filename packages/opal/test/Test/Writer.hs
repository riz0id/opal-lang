{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Writer
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Writer (testTree) where

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
  testGroup "writer"
    [ 
    ]