module Test.Expand.Lexer
  ( testTree,
  )
where

import Data.SrcLoc (SrcLoc (SrcLoc))

import Test.Compat (TestTree, testGroup)
import Test.Expand.Lexer.Core (testLexerCase)

--------------------------------------------------------------------------------

import Opal.AST.Literal (Literal (BoolLit))
import Opal.Lexer (lexBoolLit, lexStx)
import Opal.Lexer qualified as Lexer

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Lexer"
    [ testGroup
        "#%app"
        [ let expect :: Lexer.Error
              expect = Lexer.Error (SrcLoc 0 1 0) 2 Lexer.ExnEmptyApp "()"
           in testLexerCase "(#%app)" "()" lexStx $ Left expect
        , let expect :: Lexer.Error
              expect = Lexer.Error (SrcLoc 0 1 0) 2 Lexer.ExnEmptyApp "[]"
           in testLexerCase "[#%app]" "[]" lexStx $ Left expect
        ]
    , testLexerCase "#t" "#t" lexBoolLit (Right $ BoolLit True)
    , testLexerCase "#f" "#f" lexBoolLit (Right $ BoolLit False)
    ]