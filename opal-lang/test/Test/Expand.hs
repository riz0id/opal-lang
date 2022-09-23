module Test.Expand 
  ( testTree
  ) 
where

import Test.Compat (TestTree, testGroup)
import Test.Expand.Lexer qualified

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Test"
    [ Test.Expand.Lexer.testTree
    ]