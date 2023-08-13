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

import Hedgehog ((===))

import Opal.Syntax
import Opal.Syntax.TH (syntax)

import Test.Core (TestTree, testGroup, testUnit)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "th"
    [ testGroup "qval"
        [ testGroup "bool"
            [ testUnit "#f" do
                [syntax| #f |] === review syntaxBool False
            , testUnit "#t" do
                [syntax| #t |] === review syntaxBool True
            ]
        ]
    , testGroup "qvar"
        [ testGroup "bool"
            [ testUnit "#f" do
                let valB = False
                [syntax| ?valB:bool |] === review syntaxBool valB
            , testUnit "#t" do
                let valB = True
                [syntax| ?valB:bool |] === review syntaxBool valB
            ]
        ]
    ]