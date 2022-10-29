
module Test (main) where 

import Test.Compat ( testGroup, TestTree )
import Test.Tasty (defaultMain)
import Test.Opal qualified

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain testTree

testTree :: TestTree 
testTree = 
  testGroup 
    "Test"
    [ Test.Opal.testTree
    ]