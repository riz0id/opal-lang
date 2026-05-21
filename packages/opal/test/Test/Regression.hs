{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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

import Data.Default (def)
import Data.List.NonEmpty (NonEmpty (..))

import Hedgehog (annotate, evalEither, evalIO, (===))

import Opal.Common.MultiScope qualified as MultiScope
import Opal.Common.Phase (Phase (..))
import Opal.Common.Scope (Scope (..))
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Parser (runParseSyntax)
import Opal.Syntax (SExp (..))
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH (syntax)

import Test.Core (TestTree, testGroup, testUnit)

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "regression"
    [ scopeInfoInsertsIntersectionBug
    , parserIdApplicationNonExhaustive
    , multiscopeDeleteNothingWipesMultiscope
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

-- | Regression tests for
-- @review/issues/open/parser-id-application-non-exhaustive.md@.
--
-- The bug: @Opal.Parser.parseIdApplication@ matched only @lambda@,
-- @quote@, and @quote-syntax@ with no fallthrough, so any other head
-- identifier (including @letrec@ — which the expander itself emits —
-- and any user-defined operator) caused a non-exhaustive-pattern
-- runtime error. The fix adds a fallthrough that treats unrecognized
-- heads as a generic 'SApp', making the parser total.
parserIdApplicationNonExhaustive :: TestTree
parserIdApplicationNonExhaustive =
  testGroup "parser-id-application-non-exhaustive"
    [ testUnit "parses unknown identifier applications as SApp" do
        annotate "see review/issues/open/parser-id-application-non-exhaustive.md"
        -- Pre-fix: the case match in parseIdApplication had no clause
        -- for "foo", so this triggered Non-exhaustive patterns at
        -- runtime. Post-fix: falls through to SApp.
        let stx = [syntax| (foo bar) |]
        result <- evalIO (runParseSyntax def stx)
        sexp   <- evalEither result
        sexp === SApp (SVar "foo" :| [SVar "bar"])

    , testUnit "parses (letrec _) head without crashing" do
        annotate "see review/issues/open/parser-id-application-non-exhaustive.md"
        -- expandLetRec emits (letrec ...) as its output. The parser
        -- must accept it (even though the SExp evaluator has no native
        -- letrec yet — that is the separate "secondary issue" called
        -- out in the issue file).
        let stx = [syntax| (letrec x) |]
        result <- evalIO (runParseSyntax def stx)
        sexp   <- evalEither result
        sexp === SApp (SVar "letrec" :| [SVar "x"])

    , testUnit "parses (begin _) head without crashing" do
        annotate "see review/issues/open/parser-id-application-non-exhaustive.md"
        let stx = [syntax| (begin x) |]
        result <- evalIO (runParseSyntax def stx)
        sexp   <- evalEither result
        sexp === SApp (SVar "begin" :| [SVar "x"])
    ]

-- | Regression tests for
-- @review/issues/open/multiscope-delete-nothing-wipes-multiscope.md@.
--
-- The bug: @MultiScope.delete Nothing@ and @MultiScope.deletes Nothing@
-- folded onto the @empty@ accumulator instead of @mscp@, so the
-- "delete from every phase" branch returned the empty 'MultiScope'
-- for *any* input. This silently wiped per-phase scopes through
-- @ScopeInfo.insert Nothing@ and @ScopeInfo.union@ — the
-- phase-independent insert and union paths used by every
-- @syntaxScope Nothing@ call. The fix threads @mscp@ through the
-- fold.
multiscopeDeleteNothingWipesMultiscope :: TestTree
multiscopeDeleteNothingWipesMultiscope =
  testGroup "multiscope-delete-nothing-wipes-multiscope"
    [ testUnit "MultiScope.delete Nothing preserves unrelated per-phase scopes" do
        annotate "see review/issues/open/multiscope-delete-nothing-wipes-multiscope.md"
        let ph0 = Phase 0
            ph1 = Phase 1
            sc1 = Scope 4000
            sc2 = Scope 4001
            -- {0 -> {sc1}, 1 -> {sc2}}
            mscp0 = MultiScope.insert ph1 sc2 (MultiScope.insert ph0 sc1 MultiScope.empty)
            mscp1 = MultiScope.delete Nothing sc1 mscp0
        -- Pre-fix: returns empty; sc2 is wiped along with sc1.
        -- Post-fix: sc1 removed from phase 0; phase 1 still contains sc2.
        MultiScope.member ph0 sc1 mscp1 === False
        MultiScope.member ph1 sc2 mscp1 === True

    , testUnit "MultiScope.deletes Nothing preserves unrelated per-phase scopes" do
        annotate "see review/issues/open/multiscope-delete-nothing-wipes-multiscope.md"
        let ph0 = Phase 0
            ph1 = Phase 1
            sc1 = Scope 4100
            sc2 = Scope 4101
            mscp0 = MultiScope.insert ph1 sc2 (MultiScope.insert ph0 sc1 MultiScope.empty)
            mscp1 = MultiScope.deletes Nothing (ScopeSet.singleton sc1) mscp0
        MultiScope.member ph0 sc1 mscp1 === False
        MultiScope.member ph1 sc2 mscp1 === True

    , testUnit "ScopeInfo.insert Nothing preserves per-phase scopes other than the inserted one" do
        annotate "see review/issues/open/multiscope-delete-nothing-wipes-multiscope.md"
        -- This is the reachable-caller witness: ScopeInfo.insert Nothing
        -- calls MultiScope.delete Nothing on its per-phase store, which
        -- pre-fix wiped the entire MultiScope. So any phase-1 scope
        -- attached to a ScopeInfo was lost the moment any
        -- phase-independent scope was added.
        let ph        = Phase 1
            scPhase   = Scope 4200   -- per-phase
            scGlobal  = Scope 4201   -- about to add as global
            info0     = ScopeInfo.insert (Just ph) scPhase ScopeInfo.empty
            info1     = ScopeInfo.insert Nothing   scGlobal info0
            lookupPh1 = ScopeInfo.lookup (Just ph) info1
        -- The phase-1 scope must still be visible at phase 1.
        ScopeSet.member scPhase  lookupPh1 === True
        -- And the newly-added global scope is visible at every phase.
        ScopeSet.member scGlobal lookupPh1 === True
    ]
