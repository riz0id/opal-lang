module Test.Expand 
  ( testTree
  ) 
where

import Test.Compat (TestTree, testGroup)
import Test.Expand.Parse qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Expand.Parse.testTree
    ]