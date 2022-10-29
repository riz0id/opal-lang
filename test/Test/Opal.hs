{-# LANGUAGE OverloadedStrings #-}

module Test.Opal (
  testTree,
) where

import Test.Compat ( testGroup, TestTree )
import qualified Test.Opal.Read

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Opal"
    [ Test.Opal.Read.testTree
    ]
