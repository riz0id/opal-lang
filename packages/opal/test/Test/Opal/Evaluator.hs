{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Evaluator
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Evaluator
  ( testTree
  )
where

import Test.Core (TestTree, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "evaluator"
    [
    ]