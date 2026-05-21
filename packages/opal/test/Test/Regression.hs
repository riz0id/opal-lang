{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test.Regression
-- Copyright   :  (c) Jacob Leach, 2026
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Regression tests for bugs catalogued under @review/issues/@. Each
-- @testGroup@ name matches the corresponding markdown file's slug, so
-- failures point directly at the documented bug.
module Test.Regression
  ( testTree
  )
where

import Hedgehog (annotate, (===))

import Opal.Common.Phase (Phase (..))
import Opal.Common.Scope (Scope (..))
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Syntax.ScopeInfo qualified as ScopeInfo

import Test.Core (TestTree, testGroup, testUnit)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "regression"
    [ scopeInfoInsertsIntersectionBug
    ]

-- | Regression tests for
-- @review/issues/scope-info-inserts-intersection-bug.md@.
--
-- The bug: @ScopeInfo.inserts (Just ph) scps info@ was implemented with
-- @ScopeSet.intersection scps gscps@ where @ScopeSet.difference scps
-- gscps@ was intended. The intersection form silently drops every fresh
-- scope passed in (only scopes already in @gscps@ survive the
-- intersection) — so the per-phase store never accumulates new
-- information. The fix flips it to @difference@, which keeps the scopes
-- that are not already global.
scopeInfoInsertsIntersectionBug :: TestTree
scopeInfoInsertsIntersectionBug =
  testGroup "scope-info-inserts-intersection-bug"
    [ testUnit "inserts (Just ph) on empty info keeps fresh scopes" do
        annotate "see review/issues/scope-info-inserts-intersection-bug.md"
        let ph    = Phase 1
            sc    = Scope 1000
            scps  = ScopeSet.singleton sc
            info1 = ScopeInfo.inserts (Just ph) scps ScopeInfo.empty
        ScopeSet.member sc (ScopeInfo.lookup (Just ph) info1) === True

    , testUnit "inserts (Just ph) preserves already-global scopes at every phase" do
        annotate "see review/issues/scope-info-inserts-intersection-bug.md"
        let ph    = Phase 1
            sc    = Scope 2000
            info0 = ScopeInfo.insert Nothing sc ScopeInfo.empty
            info1 = ScopeInfo.inserts (Just ph) (ScopeSet.singleton sc) info0
        ScopeSet.member sc (ScopeInfo.lookup (Just ph) info1) === True
        ScopeSet.member sc (ScopeInfo.lookup Nothing   info1) === True

    , testUnit "inserts (Just ph) satisfies the lookup-union invariant" do
        annotate "see review/issues/scope-info-inserts-intersection-bug.md"
        -- The property the issue suggests checking:
        --   lookup (Just ph) (inserts (Just ph) scps info)
        --     == lookup (Just ph) info `union` scps
        let ph    = Phase 0
            scA   = Scope 3000
            scB   = Scope 3001
            info0 = ScopeInfo.insert (Just ph) scA ScopeInfo.empty
            scps  = ScopeSet.singleton scB
            info1 = ScopeInfo.inserts (Just ph) scps info0
            lhs   = ScopeInfo.lookup (Just ph) info1
            rhs   = ScopeSet.union (ScopeInfo.lookup (Just ph) info0) scps
        lhs === rhs
    ]
