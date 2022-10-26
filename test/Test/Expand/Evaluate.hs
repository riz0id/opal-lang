module Test.Expand.Syntax
  ( testTree
  ) 
where

import Test.Core 
import Data.Text (Text)
import Opal.Expand.Syntax (Syntax)
import qualified Opal.Read as Read
import Control.Exception (throwIO, ErrorCall (ErrorCall))
import qualified Opal.Expand as Expand

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Syntax" 
    []
    -- [ testProp "let-syntax" $ property do 
        
    --     _
    -- ]
