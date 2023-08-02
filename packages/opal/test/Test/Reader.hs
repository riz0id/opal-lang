{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Reader
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Reader (testTree) where

import Data.Default (Default (..))

import GHC.Stack (HasCallStack, withFrozenCallStack)

import Hedgehog (PropertyT, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (failWith)
import Hedgehog.Range qualified as Range

import Opal.Reader (runStringReader)
import Opal.Syntax (Datum (..), Syntax (..), SyntaxInfo (..))

import Test.Core (TestTree, testCase, testGroup, testUnit)

import Text.Megaparsec (errorBundlePretty)

--------------------------------------------------------------------------------

testSyntaxInfo :: SyntaxInfo
testSyntaxInfo = def
  { stx_info_source = Just "Test.Reader"
  , stx_info_srcloc = Just def
  }

runTestReader :: HasCallStack => String -> Syntax -> PropertyT IO ()
runTestReader input expected = case runStringReader "Test.Reader" input of
  Left  exn -> withFrozenCallStack (failWith Nothing (errorBundlePretty exn))
  Right stx -> withFrozenCallStack (stx === expected)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "reader"
    [ testGroup "bool"
        [ testUnit "#t" do
            runTestReader "#t" (Syntax (DatumB True) testSyntaxInfo)
        , testUnit "#f" do
            runTestReader "#f" (Syntax (DatumB False) testSyntaxInfo)
        ]
    , testCase "char" do
        char <- forAll Gen.unicode
        runTestReader ['#', '\\', char] (Syntax (DatumC char) testSyntaxInfo)
    , testCase "f32" do
        f32 <- forAll (Gen.float (Range.constant 0 10e5))
        runTestReader (show f32) (Syntax (DatumF32 f32) testSyntaxInfo)
    , testCase "i32" do
        i32 <- forAll (Gen.int32 (Range.constant 0 maxBound))
        runTestReader (show i32) (Syntax (DatumI32 i32) testSyntaxInfo)
    ]