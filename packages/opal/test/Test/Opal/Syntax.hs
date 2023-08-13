{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax
  ( testTree
  )
where

import Test.Core (TestTree, testGroup)
import Test.Opal.Syntax.TH qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "syntax"
    [ Test.Opal.Syntax.TH.testTree
    ]