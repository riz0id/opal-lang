{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Writer
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Writer
  ( testTree
  )
where


import Test.Core (TestTree, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "writer"
    [
    ]