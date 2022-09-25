
module Test 
  ( main 
  )
where 

import Test.Compat ( testGroup, TestTree )
import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

import Test.Expand qualified
import Test.Parse qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree 
testTree = 
  testGroup 
    "Test"
    [ Test.Expand.testTree
    , Test.Parse.testTree
    ]