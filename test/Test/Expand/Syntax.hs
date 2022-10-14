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

-- propLetSyntaxExpand :: Property 
-- propLetSyntaxExpand = property do 
--   _

expandOpalIO :: Text -> IO Syntax
expandOpalIO source = do 
  stx <- readOpalIO source 
  case Expand.runExpandSyntax stx of 
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx' -> pure stx'

readOpalIO :: Text -> IO Syntax
readOpalIO source =
  case Read.runRead source of
    Left exn -> throwIO (ErrorCall $ show exn)
    Right stx -> pure stx
