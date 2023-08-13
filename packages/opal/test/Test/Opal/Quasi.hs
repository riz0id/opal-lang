{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Quasi
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Quasi
  ( testTree
  )
where


import Test.Core (TestTree, testGroup)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "quasi"
    [
    ]