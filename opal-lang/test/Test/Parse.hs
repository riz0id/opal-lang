module Test.Parse
  ( testTree,
  )
where

import Control.Monad.State.Strict (get)

import Data.SrcLoc (SrcLoc (SrcLoc))

import Hedgehog (forAll, property, (===))

-- import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Compat (TestTree, testGroup, testProp)
import Test.Parse.Gen qualified as Gen

--------------------------------------------------------------------------------

import Data.Bifunctor (first)
import Data.Parse
import Opal.AST.Literal (Literal (BoolLit))
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Parse"
    [ testProp "satisfy" $ property do
        chr <- forAll Gen.unicode
        let loc = runStringParse [chr] "satisfy-parser" (satisfy (chr ==) >> get)
        Right (SrcLoc 1 1 2) === loc
    , testProp "satisfy" $ property do
        chr <- forAll Gen.unicode
        let loc = runStringParse [chr] "satisfy-parser" (satisfy (chr ==) >> get)
        Right (SrcLoc 1 1 2) === loc
    , testProp "single" $ property do
        chr <- forAll Gen.unicode
        let loc = runStringParse [chr] "single-parser" (single chr >> get)
        Right (SrcLoc 1 1 2) === loc
    , testProp "string" $ property do
        str <- forAll Gen.string'unicode
        let loc = runStringParse str "string-parser" (string str >> get)
        let len = length str
        Right (SrcLoc len 1 (1 + len)) === loc
    , testProp "whitespace" $ property do
        str <- forAll (Gen.string $ pure ' ')
        let loc = runStringParse str "whitespace-parser" (whitespace >> get)
        let len = length str
        Right (SrcLoc len 1 (1 + len)) === loc
    , testTreeScan
    ]

testTreeScan :: TestTree
testTreeScan =
  testGroup
    "Scan"
    [ testProp "scan" $ property do
        chr <- forAll Gen.unicode
        str <- forAll (Gen.string $ pure chr)
        sfx <- forAll (Gen.string $ Gen.except chr Gen.unicode)
        let loc = runStringParse (str ++ sfx) "scan-parser" (scan (chr ==) get)
        let len = length str
        Right (SrcLoc len 1 (1 + len)) === loc
    , testProp "scan1" $ property do
        chr <- forAll Gen.unicode
        str <- forAll (Gen.string $ pure chr)
        sfx <- forAll (Gen.string $ Gen.except chr Gen.unicode)
        let loc = runStringParse (str ++ sfx) "scan-parser" (scan1 (chr ==) get)
        let len = length str
        if len == 0
          then do
            Left (SrcLoc 0 1 1) === first exn'begin loc
            Left (SrcLoc 0 1 1) === first exn'posn loc
            Left (Symbol.pack sfx) === first exn'span loc
          else Right (SrcLoc len 1 (1 + len)) === loc
    ]

-- [ testGroup
--     "#%app"
--     [ let expect :: Lexer.Error
--           expect = Lexer.Error (SrcLoc 0 1 0) 2 Lexer.ExnEmptyApp "()"
--        in testLexerCase "(#%app)" "()" lexStx $ Left expect
--     , let expect :: Lexer.Error
--           expect = Lexer.Error (SrcLoc 0 1 0) 2 Lexer.ExnEmptyApp "[]"
--        in testLexerCase "[#%app]" "[]" lexStx $ Left expect
--     ]
-- , testLexerCase "#t" "#t" lexBoolLit (Right $ BoolLit True)
-- , testLexerCase "#f" "#f" lexBoolLit (Right $ BoolLit False)
-- ]