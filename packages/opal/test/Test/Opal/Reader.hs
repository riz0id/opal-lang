{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Reader
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Reader
  ( testTree
  )
where

import Control.Lens ((^.))

import Data.Default (Default (..))

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Hedgehog (PropertyT, annotate, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range

import Opal.Common.SourceInfo (SourceInfo(..))
import Opal.Reader (runStringReader)
import Opal.Syntax (Syntax (..), SyntaxInfo (..), syntaxInfo)

import Test.Core (TestTree, testCase, testGroup, testUnit)

import Text.Megaparsec (errorBundlePretty)

--------------------------------------------------------------------------------

testSyntaxInfo :: SyntaxInfo
testSyntaxInfo = def { stx_info_source = Just (SourceInfo "Test.Opal.Reader" def) }

runTestReader :: HasCallStack => String -> Syntax -> PropertyT IO ()
runTestReader input expected = do
  case runStringReader "Test.Opal.Reader" input of
    Left  exn -> withFrozenCallStack (failWith Nothing (errorBundlePretty exn))
    Right stx -> do
      annotate ("Reader result: " ++ show stx)
      annotate ("Lexical info: " ++ show (stx ^. syntaxInfo))
      withFrozenCallStack (stx === expected)

-- | Assert that the reader accepts @input@ without checking the
-- resulting 'Syntax' shape. Useful when source positions in the
-- expected value would otherwise be tedious to construct (e.g. list
-- forms with comment whitespace).
runTestReaderOk :: HasCallStack => String -> PropertyT IO ()
runTestReaderOk input =
  case runStringReader "Test.Opal.Reader" input of
    Left  exn -> withFrozenCallStack (failWith Nothing (errorBundlePretty exn))
    Right _   -> pure ()

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "reader"
    [ testGroup "bool"
        [ testUnit "#t" do
            runTestReader "#t" (SyntaxB True testSyntaxInfo)
        , testUnit "#f" do
            runTestReader "#f" (SyntaxB False testSyntaxInfo)
        ]
    , testCase "char" do
        char <- forAll Gen.unicode
        runTestReader ['#', '\\', char] (SyntaxC char testSyntaxInfo)
    , testCase "f32" do
        f32 <- forAll (Gen.float (Range.constant 0 10e5))
        runTestReader (show f32) (SyntaxF32 f32 testSyntaxInfo)
    , testCase "i32" do
        i32 <- forAll (Gen.int32 (Range.constant 0 maxBound))
        runTestReader (show i32) (SyntaxI32 i32 testSyntaxInfo)
    , testGroup "comments"
        [ testUnit "line comment before token" do
            -- Token is past the comment, so its source position
            -- differs from testSyntaxInfo's default — use the
            -- position-agnostic helper.
            runTestReaderOk "; greeting\n#t"
        , testUnit "trailing line comment" do
            runTestReader "#t ; trailing\n" (SyntaxB True testSyntaxInfo)
        , testUnit "double-semicolon line comment (convention)" do
            runTestReaderOk ";; convention\n#t"
        , testUnit "block comment" do
            runTestReaderOk "#| ignored |# #t"
        , testUnit "nested block comment" do
            runTestReaderOk "#| outer #| inner |# still outer |# #t"
        , testUnit "comment between list elements" do
            -- The load-bearing case: confirms readEnclosed's internal
            -- whitespace skippers also handle comments.
            runTestReaderOk "(#t ;; mid-list\n #f)"
        , testUnit "comment immediately before close paren" do
            runTestReaderOk "(#t #| block |#)"
        ]
    ]