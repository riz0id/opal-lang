{-# LANGUAGE QuasiQuotes #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Opal.Syntax.TH
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Test.Opal.Syntax.TH
  ( testTree
  )
where

import Control.Lens (review)

import Hedgehog ((===), forAll, failure)

import Opal.Syntax
import Opal.Syntax.TH (syntax)

import Test.Core (TestTree, testGroup, testUnit, testCase)
import qualified Test.Opal.Common.Symbol.Gen as Gen
import Data.Default (Default(..))

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "th"
    [ testGroup "expressions"
        [ testGroup "lit"
            [ testUnit "#f" do
                [syntax| #f |] === review syntaxBool False
            , testUnit "#t" do
                [syntax| #t |] === review syntaxBool True
            ]
        , testGroup "var"
            [ testUnit "#f" do
                let valB = False
                [syntax| ?valB:bool |] === review syntaxBool valB
            , testUnit "#t" do
                let valB = True
                [syntax| ?valB:bool |] === review syntaxBool valB
            ]
        ]
    , testGroup "patterns"
        [ testGroup "var"
            [ testCase "identifier" do
                id1 <- forAll (fmap (`Identifier` def) Gen.symbol)
                case [syntax| ?id1:id |] of
                  [syntax| ?id2:id |] -> id1 === id2
                  _ -> failure
            , testCase "symbol" do
                s1 <- forAll Gen.symbol
                case [syntax| ?s1:symbol |] of
                  [syntax| ?s2:symbol |] -> s1 === s2
                  _ -> failure
            , testCase "list" do
                ida1 <- forAll (fmap (`Identifier` def) Gen.symbol)
                idb1 <- forAll (fmap (`Identifier` def) Gen.symbol)
                case [syntax| (?ida1:id ?idb1:id) |] of
                  [syntax| (?ida2:id ?idb2:id) |] -> do
                    ida1 === ida2
                    idb1 === idb2
                  _ -> failure
            ]
        ]
    ]