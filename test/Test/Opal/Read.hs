{-# LANGUAGE OverloadedStrings #-}

module Test.Opal.Read (
  testTree,
) where

import Data.SrcLoc qualified as SrcLoc

import Hedgehog ((===))

import Opal.Expand.Syntax (StxCtx (StxCtx), Syntax (StxBool))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Read qualified as Read

import Test.Core ( testGroup, TestTree, testCase )

-- Helpers ---------------------------------------------------------------------

makeStxBool :: Bool -> Syntax
makeStxBool = StxBool (StxCtx (Just SrcLoc.empty) MultiScopeSet.empty)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Read"
    [ testGroup
        "False"
        [ testCase "#f" (Read.runRead "#f" === Right (makeStxBool False))
        , testCase "#F" (Read.runRead "#F" === Right (makeStxBool False))
        ]
    , testGroup
        "True"
        [ testCase "#t" (Read.runRead "#t" === Right (makeStxBool True))
        , testCase "#T" (Read.runRead "#T" === Right (makeStxBool True))
        ]
    ]
