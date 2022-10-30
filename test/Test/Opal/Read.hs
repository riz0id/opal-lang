{-# LANGUAGE OverloadedStrings #-}

module Test.Opal.Read (
  testTree,
) where

import Data.SrcLoc qualified as SrcLoc

import Hedgehog ((===))

import Opal.Expand.Syntax (StxCtx (StxCtx), Syntax (StxBool, StxVoid))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

import Opal.Read qualified as Read

import Test.Core (TestTree, testCase, testGroup)

-- Helpers ---------------------------------------------------------------------

makeStxCtx :: StxCtx
makeStxCtx =
  let phase = MultiScopeSet.Phase 0
      scope = ScopeSet.singleton (ScopeSet.ScopeId 0)
   in StxCtx (Just SrcLoc.empty) (MultiScopeSet.singleton phase scope)

makeStxVoid :: Syntax
makeStxVoid = StxVoid makeStxCtx

makeStxBool :: Bool -> Syntax
makeStxBool = StxBool makeStxCtx

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup
    "Read"
    [ testCase "#<void>" (Read.runRead "#<void>" === Right makeStxVoid)
    , testGroup
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
