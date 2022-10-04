
module Test 
  ( main 
  )
where 

import Test.Compat ( testGroup, TestTree )
import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

import Test.Expand qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree 
testTree = 
  testGroup 
    "Test"
    [ Test.Expand.testTree
    ]