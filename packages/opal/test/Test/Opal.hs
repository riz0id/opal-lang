{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal
  ( testTree
  )
where

import Test.Tasty (TestTree, testGroup)

import Test.Opal.Common qualified
import Test.Opal.Evaluator qualified
import Test.Opal.Memory.Buffer qualified
import Test.Opal.Syntax qualified
import Test.Opal.Reader qualified
import Test.Opal.Writer qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "opal"
    [ Test.Opal.Common.testTree
    , Test.Opal.Evaluator.testTree
    , Test.Opal.Memory.Buffer.testTree
    , Test.Opal.Syntax.testTree
    , Test.Opal.Reader.testTree
    , Test.Opal.Writer.testTree
    ]