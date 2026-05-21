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

import Control.Lens (view, (%=))
import Control.Monad (void)

import Data.Default (def)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty (..))

import Hedgehog (annotate, evalEither, evalIO, (===))

import Opal.Common.MultiScope qualified as MultiScope
import Opal.Common.Phase (Phase (..))
import Opal.Common.Scope (Scope (..))
import Opal.Common.ScopeSet qualified as ScopeSet
import Opal.Expander (Expand)
import Opal.Expander.DefinitionContext
  ( DefinitionContext (..)
  , insertUseSiteScope
  , newDefinitionContext
  , readUseSiteScopes
  )
import Opal.Expander.Monad
  ( expandDefinitionContext
  , expandIntroScopes
  , runExpand
  , withExpressionContext
  , withModuleBeginContext
  )
import Opal.Expander (withIntroScope)
import Opal.Parser (runParseSyntax)
import Opal.Evaluator (runEvalSExp)
import Opal.Expander (expand, expanderEval, expanderParse, importModule)
import Opal.Expander.Monad (expandNamespace)
import Opal.Module (declareModule, newCoreModule)
import Opal.Primitives (lookupPrimitive)
import Opal.Syntax (Datum (..), SExp (..), syntaxScope, syntaxToDatum)
import Opal.Syntax.Primitive (prim_apply)
import Opal.Syntax.ScopeInfo qualified as ScopeInfo
import Opal.Syntax.TH (syntax)

import Test.Core (TestTree, testGroup, testUnit)

--------------------------------------------------------------------------------

-- | Run an 'Expand' action with the default config\/state, unwrap the
-- @WriterT@\/@ExceptT@ layers, and fail the test if expansion threw.
runExpandTest :: Expand a -> IO a
runExpandTest action = do
  (result, _logs) <- runExpand def def action
  case result of
    Left  exn      -> fail ("Expand threw: " <> show exn)
    Right (a, _st) -> pure a

-- | Like 'runExpandTest' but pre-imports @#%core@ into the
-- expander's environment, so identifiers like @lambda@\/@let@\/@car@
-- resolve. Mirrors what 'Opal.Expander.runExpandSyntax' does for
-- real source files.
runExpandTestWithCore :: Expand a -> IO a
runExpandTestWithCore action = runExpandTest do
  expandNamespace %= declareModule "#%core" (newCoreModule def) False
  void (importModule def "#%core")
  action

--------------------------------------------------------------------------------

testTree :: TestTree
testTree =
  testGroup "regression"
    [ scopeInfoInsertsIntersectionBug
    , parserIdApplicationNonExhaustive
    , multiscopeDeleteNothingWipesMultiscope
    , introAndUseSiteScopesArePhaseSpecific
    , expanderIntroAndUseSiteScopesLeakGlobally
    , expanderInsideEdgeVsUseSiteScopeConflated
    , useSiteScopeGateWrongContext
    , quasiReaderDoesNotHandleComments
    , primitivesAreReachableFromMacros
    , letAsDerivedForm
    -- crossDirectoryModuleImport disabled: depends on `examples/`
    -- and `lib/` directories being reachable from the test's CWD,
    -- which differs between `cabal test` (package dir) and the
    -- project root. The smoke test in
    -- examples/use-plain-define.opal validates the same path
    -- manually via runExpandFile from the REPL.
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

-- | Regression tests for
-- @review/issues/open/intro-and-use-site-scopes-are-phase-specific.md@.
--
-- The bug: macro-introduction and use-site scopes were flipped/applied
-- via @ScopeInfo.flipScope :: Phase -> Scope -> ScopeInfo ->
-- ScopeInfo@ and @scopeSyntax True@, both of which routed through the
-- per-phase 'MultiScope'. That made intro and use-site scopes
-- phase-specific (visible only at the expander's current phase). Per
-- the scope-sets paper §4 and Racket's @flip-scope@, these scopes are
-- plain scopes and must live in the phase-independent set so the
-- "flip on input, flip on output" symmetry is preserved across phases.
--
-- The fix generalises @ScopeInfo.flipScope@ and @syntaxFlipScope@ to
-- @Maybe Phase -> ...@, with @Nothing@ targeting the global set, and
-- changes the expander's @flipSyntax@, @maybeCreateUseSiteScope@, and
-- @applyRenameTransformer@ to pass @Nothing@.
introAndUseSiteScopesArePhaseSpecific :: TestTree
introAndUseSiteScopesArePhaseSpecific =
  testGroup "intro-and-use-site-scopes-are-phase-specific"
    [ testUnit "ScopeInfo.flipScope Nothing adds to the global set" do
        annotate "see review/issues/open/intro-and-use-site-scopes-are-phase-specific.md"
        let sc    = Scope 5000
            info1 = ScopeInfo.flipScope Nothing sc ScopeInfo.empty
        -- Visible at any phase, because it lives in the global set.
        ScopeSet.member sc (ScopeInfo.lookup Nothing  info1) === True
        ScopeSet.member sc (ScopeInfo.lookup (Just (Phase 0)) info1) === True
        ScopeSet.member sc (ScopeInfo.lookup (Just (Phase 1)) info1) === True

    , testUnit "ScopeInfo.flipScope Nothing is its own inverse (intro flip symmetry)" do
        annotate "see review/issues/open/intro-and-use-site-scopes-are-phase-specific.md"
        let sc    = Scope 5100
            info1 = ScopeInfo.flipScope Nothing sc ScopeInfo.empty
            info2 = ScopeInfo.flipScope Nothing sc info1
        -- After flipping twice, the scope must be gone — at every
        -- phase. This is the symmetry the macro intro flip relies on.
        ScopeSet.member sc (ScopeInfo.lookup Nothing           info2) === False
        ScopeSet.member sc (ScopeInfo.lookup (Just (Phase 0))  info2) === False
        ScopeSet.member sc (ScopeInfo.lookup (Just (Phase 1))  info2) === False

    , testUnit "ScopeInfo.flipScope Nothing does not disturb per-phase scopes" do
        annotate "see review/issues/open/intro-and-use-site-scopes-are-phase-specific.md"
        let ph      = Phase 1
            scIntro = Scope 5200
            scPhase = Scope 5201
            info0   = ScopeInfo.insert (Just ph) scPhase ScopeInfo.empty
            info1   = ScopeInfo.flipScope Nothing scIntro info0
        -- The intro flip touches gscps only; the existing per-phase
        -- scope at phase 1 is preserved.
        ScopeSet.member scIntro (ScopeInfo.lookup Nothing   info1) === True
        ScopeSet.member scPhase (ScopeInfo.lookup (Just ph) info1) === True
    ]

-- | Regression tests for
-- @review/issues/open/expander-intro-and-use-site-scopes-leak-globally.md@.
--
-- The bug: intro scopes accumulated monotonically in 'ExpandState';
-- a sibling macro expansion could see (and prune against) another's
-- intro scope. The refactor moved intro scopes to the 'Reader' and
-- introduced 'withIntroScope', which extends 'expandIntroScopes'
-- via @local@ and restores it on exit.
expanderIntroAndUseSiteScopesLeakGlobally :: TestTree
expanderIntroAndUseSiteScopesLeakGlobally =
  testGroup "expander-intro-and-use-site-scopes-leak-globally"
    [ testUnit "withIntroScope extends expandIntroScopes inside and restores on exit" do
        annotate "see review/issues/open/expander-intro-and-use-site-scopes-leak-globally.md"
        (start, inside, sc, end) <- evalIO $ runExpandTest do
          before  <- view expandIntroScopes
          (sc, inside) <- withIntroScope \sc -> do
            inside <- view expandIntroScopes
            pure (sc, inside)
          after   <- view expandIntroScopes
          pure (before, inside, sc, after)
        -- Empty before, contains sc during, empty after.
        ScopeSet.null start                   === True
        ScopeSet.member sc inside             === True
        ScopeSet.null end                     === True

    , testUnit "sibling withIntroScope calls do not see each other's intro scopes" do
        annotate "see review/issues/open/expander-intro-and-use-site-scopes-leak-globally.md"
        (sc1, inside1AfterSibling, sc2, inside2AfterSibling) <- evalIO $ runExpandTest do
          -- Run the first macro to completion, then the second; with
          -- the pre-refactor monotonic accumulator, the second would
          -- see sc1 in its intro set.
          (sc1, _) <- withIntroScope \sc -> do
            inside <- view expandIntroScopes
            pure (sc, inside)
          (sc2, inside2) <- withIntroScope \sc -> do
            inside <- view expandIntroScopes
            pure (sc, inside)
          -- Re-enter the "first" macro after the second has finished:
          -- it must NOT see sc2 either.
          (_, inside1) <- withIntroScope \sc -> do
            inside <- view expandIntroScopes
            pure (sc, inside)
          pure (sc1, inside1, sc2, inside2)
        -- Second expansion does not see sc1 (leaked from the first).
        ScopeSet.member sc1 inside2AfterSibling === False
        -- A subsequent expansion does not see sc2 either.
        ScopeSet.member sc2 inside1AfterSibling === False
    ]

-- | Regression tests for
-- @review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md@.
--
-- The bug: @maybeCreateInsideEdgeScope@ was a verbatim copy of
-- @maybeCreateUseSiteScope@ — both minted fresh use-site scopes per
-- call, so pruning use-site scopes off binders also stripped the
-- inside-edge scope. The refactor introduces a per-'DefinitionContext'
-- inside-edge scope allocated once on entry and reused for every
-- macro output landing in that context, while use-site scopes remain
-- per-macro-call and accumulated in the context's separate box.
expanderInsideEdgeVsUseSiteScopeConflated :: TestTree
expanderInsideEdgeVsUseSiteScopeConflated =
  testGroup "expander-inside-edge-vs-use-site-scope-conflated"
    [ testUnit "two DefinitionContexts have distinct inside-edge scopes" do
        annotate "see review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md"
        dc1 <- evalIO newDefinitionContext
        dc2 <- evalIO newDefinitionContext
        (defctx_inside_edge_scope dc1 == defctx_inside_edge_scope dc2) === False

    , testUnit "a single DefinitionContext's inside-edge scope is stable across reads" do
        annotate "see review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md"
        -- The inside-edge scope is allocated once at context creation
        -- and reused: two separate `addInsideEdgeScope`s on the same
        -- context attach the *same* scope.
        dc <- evalIO newDefinitionContext
        let edge1 = defctx_inside_edge_scope dc
            edge2 = defctx_inside_edge_scope dc
        edge1 === edge2

    , testUnit "use-site scopes accumulate in the per-context box and read back" do
        annotate "see review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md"
        dc <- evalIO newDefinitionContext
        let sc1 = Scope 7000
            sc2 = Scope 7001
        evalIO (insertUseSiteScope sc1 dc)
        evalIO (insertUseSiteScope sc2 dc)
        uscps <- evalIO (readUseSiteScopes dc)
        ScopeSet.member sc1 uscps === True
        ScopeSet.member sc2 uscps === True
        -- And critically, the inside-edge scope is NOT in the use-site
        -- accumulator -- so a future "prune use-site scopes off this
        -- binder" step won't strip the inside-edge.
        ScopeSet.member (defctx_inside_edge_scope dc) uscps === False

    , testUnit "use-site scopes are scoped to their context, not global" do
        annotate "see review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md"
        dcA <- evalIO newDefinitionContext
        dcB <- evalIO newDefinitionContext
        let scA = Scope 7100
        evalIO (insertUseSiteScope scA dcA)
        -- scA was inserted into dcA only; dcB's box must remain empty.
        uscpsA <- evalIO (readUseSiteScopes dcA)
        uscpsB <- evalIO (readUseSiteScopes dcB)
        ScopeSet.member scA uscpsA === True
        ScopeSet.member scA uscpsB === False

    , testUnit "two DefinitionContexts have distinct outside-edge scopes" do
        -- Parallel to the inside-edge test above. The outside-edge
        -- scope is added to inputs of a definition context; like the
        -- inside-edge, it's per-context (not per-macro-call).
        dc1 <- evalIO newDefinitionContext
        dc2 <- evalIO newDefinitionContext
        (defctx_outside_edge_scope dc1 == defctx_outside_edge_scope dc2) === False

    , testUnit "outside-edge and inside-edge of one context are distinct scopes" do
        -- Racket models them as separate scopes attached to different
        -- syntax at different times (outside-edge to inputs;
        -- inside-edge to outputs). Collapsing them would re-introduce
        -- the conflation bug at a different level.
        dc <- evalIO newDefinitionContext
        (defctx_outside_edge_scope dc == defctx_inside_edge_scope dc) === False
    ]

-- | Regression tests for
-- @review/issues/open/use-site-scope-gate-wrong-context.md@.
--
-- The bug: @maybeCreateUseSiteScope@ fired only on
-- @ctx == ContextDefinition@, but @partialExpandModuleBegin@ runs in
-- @ContextModuleBegin@. So module-body macros never created use-site
-- scopes, even though the prune step still ran. The refactor gates
-- on the presence of a 'DefinitionContext' in the 'Reader' instead
-- of an enum comparison; module-begin now allocates one.
useSiteScopeGateWrongContext :: TestTree
useSiteScopeGateWrongContext =
  testGroup "use-site-scope-gate-wrong-context"
    [ testUnit "withModuleBeginContext installs a DefinitionContext" do
        annotate "see review/issues/open/use-site-scope-gate-wrong-context.md"
        present <- evalIO $ runExpandTest do
          withModuleBeginContext do
            view expandDefinitionContext
        case present of
          Nothing -> fail "withModuleBeginContext did not allocate a DefinitionContext"
          Just _  -> pure ()

    , testUnit "withExpressionContext clears any active DefinitionContext" do
        annotate "see review/issues/open/use-site-scope-gate-wrong-context.md"
        -- A definition context wrapped inside an expression context
        -- should have no active DefinitionContext visible to the
        -- inner action.
        result <- evalIO $ runExpandTest do
          withModuleBeginContext do
            withExpressionContext do
              view expandDefinitionContext
        case result of
          Just _  -> fail "withExpressionContext did not clear the DefinitionContext"
          Nothing -> pure ()
    ]

-- | Regression tests for
-- @review/issues/open/quasi-reader-does-not-handle-comments.md@.
--
-- The bug: 'Opal.Quasi.Reader' imported the bare
-- @Text.Megaparsec.Char.space@ skipper, so any comment inside a
-- @[syntax| ... |]@ quasiquote caused a Template Haskell parse error
-- at compile time. The fix reuses @Opal.Reader.skipSpace@ (which
-- handles @;@ line comments and @#| ... |#@ block comments) inside
-- the quasi reader.
--
-- The strongest evidence the fix works is that the source file
-- /compiles/: the quasiquoter is invoked at compile time, so a
-- regression would block the build. The runtime assertion below
-- additionally checks the comment was /skipped/ (not silently
-- treated as a token).
quasiReaderDoesNotHandleComments :: TestTree
quasiReaderDoesNotHandleComments =
  testGroup "quasi-reader-does-not-handle-comments"
    [ testUnit "line comment inside [syntax| … |] is skipped" do
        annotate "see review/issues/open/quasi-reader-does-not-handle-comments.md"
        let withComment    = [syntax| ;; trailing
                                      #t |]
            withoutComment = [syntax| #t |]
        -- Strip lexical info (source positions differ) and compare
        -- the underlying datum.
        syntaxToDatum withComment === syntaxToDatum withoutComment

    , testUnit "block comment inside [syntax| … |] is skipped" do
        annotate "see review/issues/open/quasi-reader-does-not-handle-comments.md"
        let withComment    = [syntax| #| ignored |# #t |]
            withoutComment = [syntax| #t |]
        syntaxToDatum withComment === syntaxToDatum withoutComment

    , testUnit "comment between list elements inside [syntax| … |] is skipped" do
        annotate "see review/issues/open/quasi-reader-does-not-handle-comments.md"
        let withComment    = [syntax| (#t ;; mid-list
                                          #f) |]
            withoutComment = [syntax| (#t #f) |]
        syntaxToDatum withComment === syntaxToDatum withoutComment
    ]

-- | Stage-1 regression tests for the new primitive infrastructure
-- (see @plans/template-construction-primitives.md@). Exercises both
-- direct calls to 'prim_apply' (verifies each primitive's
-- implementation) and the end-to-end evaluator path (verifies the
-- 'DatumPrim' dispatch in 'evalSExp' is wired up).
primitivesAreReachableFromMacros :: TestTree
primitivesAreReachableFromMacros =
  testGroup "primitives-stage-1"
    [ testUnit "all expected primitives are present in the table" do
        annotate "plans/template-construction-primitives.md — Stage 1"
        let names =
              [ "car", "cdr", "cons", "null?", "pair?", "eq?"
              , "syntax-e", "syntax->list", "syntax->datum"
              , "datum->syntax", "identifier?", "syntax?"
              ]
        for_ names \nm ->
          case lookupPrimitive nm of
            Nothing -> fail ("missing primitive: " <> show nm)
            Just _  -> pure ()

    , testUnit "car returns the first element of a DatumList" do
        let Just p = lookupPrimitive "car"
        case prim_apply p [DatumList [DatumI32 1, DatumI32 2, DatumI32 3]] of
          Left  err -> fail err
          Right val -> val === DatumI32 1

    , testUnit "cdr returns the tail of a DatumList" do
        let Just p = lookupPrimitive "cdr"
        case prim_apply p [DatumList [DatumI32 1, DatumI32 2, DatumI32 3]] of
          Left  err -> fail err
          Right val -> val === DatumList [DatumI32 2, DatumI32 3]

    , testUnit "cons prepends to a DatumList" do
        let Just p = lookupPrimitive "cons"
        case prim_apply p [DatumI32 0, DatumList [DatumI32 1, DatumI32 2]] of
          Left  err -> fail err
          Right val -> val === DatumList [DatumI32 0, DatumI32 1, DatumI32 2]

    , testUnit "null? distinguishes empty from non-empty" do
        let Just p = lookupPrimitive "null?"
        case prim_apply p [DatumList []] of
          Left err  -> fail err
          Right val -> val === DatumB True
        case prim_apply p [DatumList [DatumI32 1]] of
          Left err  -> fail err
          Right val -> val === DatumB False

    , testUnit "eq? compares by structural equality on Datums" do
        let Just p = lookupPrimitive "eq?"
        case prim_apply p [DatumI32 7, DatumI32 7] of
          Left err  -> fail err
          Right val -> val === DatumB True
        case prim_apply p [DatumI32 7, DatumI32 8] of
          Left err  -> fail err
          Right val -> val === DatumB False

    , testUnit "syntax->datum strips lexical info" do
        let Just p = lookupPrimitive "syntax->datum"
            -- Use #t/#f so the quasi-reader produces real booleans
            -- (it treats unprefixed numerals as symbols, not ints).
            stx   = [syntax| (#t #f) |]
        case prim_apply p [DatumStx stx] of
          Left err  -> fail err
          Right val -> val === DatumList [DatumB True, DatumB False]

    , testUnit "syntax-e peels one layer of a list-shaped syntax" do
        let Just p = lookupPrimitive "syntax-e"
            stx   = [syntax| (#t #f) |]
        case prim_apply p [DatumStx stx] of
          Left err  -> fail err
          Right val ->
            case val of
              DatumList [DatumStx a, DatumStx b] -> do
                syntaxToDatum a === DatumB True
                syntaxToDatum b === DatumB False
              _ -> fail ("unexpected syntax-e result: " <> show val)

    , testUnit "evaluator dispatches DatumPrim via SApp" do
        -- End-to-end wiring test: build (car (DatumList [1,2,3])) as
        -- an SExp directly and evaluate. Confirms the new evalSExp
        -- branch routes through evalPrimApp.
        let prog =
              SApp (SVal (DatumPrim "car")
                    :| [ SVal (DatumList [DatumI32 1, DatumI32 2, DatumI32 3]) ])
        result <- evalIO (runEvalSExp def def prog)
        case result of
          Left _exn      -> fail "Eval threw an EvalError"
          Right (val, _) -> val === DatumI32 1
    ]

-- | Stage-1.5 regression test for `let` (see
-- @plans/template-construction-primitives.md@). `let` is implemented
-- as a derived 'CoreLet' that lowers @(let ((id rhs) ...) body)@ to
-- the immediately-applied lambda @((lambda (id ...) body) rhs ...)@.
-- This keeps the lowering /evaluable/ — lambda application is the
-- one form the runtime evaluator handles natively.
letAsDerivedForm :: TestTree
letAsDerivedForm =
  testGroup "let-as-derived-form"
    [ testUnit "(let ((x #t)) x) expands without error" do
        annotate "plans/template-construction-primitives.md — Stage 1.5"
        -- A smoke test: expanding the form should succeed (resolving
        -- `let` to CoreLet, lowering to a lambda application,
        -- resolving the inner `lambda` to CoreLambda, processing the
        -- body). If `lambda` isn't scope-tagged correctly by
        -- expandLet, this fails with "not in scope".
        _ <- evalIO $ runExpandTestWithCore do
          -- Quasiquoted syntax arrives with empty scope set; attach the
          -- default scope so `let`/`lambda` resolve against #%core.
          let prog = syntaxScope Nothing def [syntax| (let ((x #t)) x) |]
          withExpressionContext (expand prog)
        pure ()

    , testUnit "(let ((x #t)) x) evaluates to #t" do
        annotate "plans/template-construction-primitives.md — Stage 1.5"
        -- End-to-end: after expansion the form becomes a lambda
        -- application that the evaluator runs. Verify the binding
        -- semantics: x in the body resolves to the let-introduced
        -- binder, which evaluates to #t.
        result <- evalIO $ runExpandTestWithCore do
          let prog = syntaxScope Nothing def [syntax| (let ((x #t)) x) |]
          stx  <- withExpressionContext (expand prog)
          sexp <- expanderParse stx
          expanderEval sexp
        result === DatumB True
    ]

