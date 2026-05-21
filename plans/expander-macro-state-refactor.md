# Plan — unified macro-state refactor

Covers all three remaining open issues in a single coordinated change:

- `review/issues/open/expander-intro-and-use-site-scopes-leak-globally.md`
- `review/issues/open/expander-inside-edge-vs-use-site-scope-conflated.md`
- `review/issues/open/use-site-scope-gate-wrong-context.md`

## Context

Today, intro and use-site scopes are *global monotonic accumulators* on
`ExpandState` (`expand_intro_scopes`, `expand_usage_scopes`). Every
macro expansion appends to them; nothing ever removes. The result is
that pruning steps (`expandQuoteSyntax`,
`partialExpandModuleBegin`'s define branches) read these globals and
strip *all historical* scopes, which cross-contaminates unrelated
macros.

Inside-edge and use-site are a verbatim copy of each other
(`maybeCreateInsideEdgeScope` ≡ `maybeCreateUseSiteScope`), and the
gate `ctx == ContextDefinition` excludes the most common case (module
bodies). The unified fix scopes both kinds of state to the macro
invocation / definition context they actually belong to.

## Target architecture

Three pieces:

### 1. `DefinitionContext` record (new type, new module)

```haskell
-- packages/opal/src/Opal/Expander/DefinitionContext.hs (NEW)
data DefinitionContext = DefinitionContext
  { defctx_use_site_scopes   :: {-# UNPACK #-} !(IORef ScopeSet)
    -- ^ Accumulates use-site scopes produced by every macro call in this
    --   definition context. Mutable because multiple macro expansions
    --   in the same context all push into the same box, regardless of
    --   nesting depth.
  , defctx_inside_edge_scope :: {-# UNPACK #-} !Scope
    -- ^ One fresh scope, allocated once on entry to the context. Every
    --   macro output in this context gets this same scope attached.
  }
```

The `IORef` is required because nested macro expansions need to mutate
a shared accumulator, but the surrounding *outer* expansion still
needs to see the accumulated set when it later prunes. `Reader`-only
doesn't work for the use-site set (mutation isn't visible up the call
stack); `State`-only doesn't work either (sibling expansions would
clobber each other). Per-context `IORef` is what Racket uses (`box`)
and is the right answer.

The inside-edge scope, by contrast, is *immutable* per context —
allocate it once when entering the context, share it for all macro
outputs landing there.

### 2. `ExpandConfig` changes (Reader)

```haskell
data ExpandConfig = ExpandConfig
  { …
  , expand_definition_context :: Maybe DefinitionContext   -- NEW
  , expand_intro_scopes       :: ScopeSet                  -- MOVED FROM STATE
  }
```

- `expand_definition_context`: `Just dc` inside any definition context
  (module-body, internal `begin`, top-level), `Nothing` for expression
  contexts. Macros use it to (a) gate use-site scope creation, (b)
  gate inside-edge attachment, (c) find the right pruning box.
- `expand_intro_scopes`: the *currently in scope* intro scopes —
  extended via `local` on entry to each macro expansion, automatically
  restored on exit. No mutable state. Each macro sees only its own
  ancestors' intro scopes, not its siblings'.

### 3. `ExpandState` changes

```haskell
data ExpandState = ExpandState
  { expand_binding_store :: BindingStore
  , expand_environment   :: Environment
  , expand_namespace     :: Namespace
  -- expand_intro_scopes  REMOVED (now in Reader)
  -- expand_usage_scopes  REMOVED (now in DefinitionContext IORef)
  }
```

Both global accumulators go away entirely.

## Concrete code path changes

### New helpers

```haskell
-- All in Opal.Expander, replacing newIntroScope / newUsageScope:

withIntroScope :: (Scope -> Expand a) -> Expand a
withIntroScope k = do
  sc <- newScope
  local (over expandIntroScopes (ScopeSet.insert sc)) (k sc)

addUseSiteScope :: Syntax -> Expand Syntax
addUseSiteScope s = do
  view expandDefinitionContext >>= \case
    Nothing -> pure s            -- not in a definition context: no use-site
    Just dc -> do
      usc <- newScope
      liftIO (modifyIORef' (defctx_use_site_scopes dc) (ScopeSet.insert usc))
      pure (syntaxScope Nothing usc s)

addInsideEdgeScope :: Syntax -> Expand Syntax
addInsideEdgeScope s =
  view expandDefinitionContext >>= \case
    Nothing -> pure s            -- not in a definition context: no inside-edge
    Just dc -> pure (syntaxScope Nothing (defctx_inside_edge_scope dc) s)

withDefinitionContext :: Expand a -> Expand a    -- already exists; rewritten
withDefinitionContext k = do
  box  <- liftIO (newIORef ScopeSet.empty)
  edge <- newScope
  let dc = DefinitionContext box edge
  local ( set  expandDefinitionContext (Just dc)
        . set  expandContext           ContextDefinition )
        k
```

Analogous `withModuleBeginContext` and `withTopLevelContext` also
allocate a fresh `DefinitionContext` (they too are definition contexts
per the `use-site-scope-gate-wrong-context` issue).

`withExpressionContext` and `withModuleContext` *clear*
`expandDefinitionContext` to `Nothing` (no definitions allowed; macros
expanded in these contexts get no use-site/inside-edge scope).

### Rewritten `applyTransformer`

```haskell
applyTransformer t stx = withIntroScope \introScope -> do
  introStx <- flipSyntax introScope stx
  inputStx <- addUseSiteScope introStx

  transformed <- … run the macro body …

  resultStx <- flipSyntax introScope transformed
  postStx   <- addInsideEdgeScope resultStx
  pure postStx
```

The `where`-clause helpers go away. The intro scope is visible only
inside `k` (no leak). Use-site/inside-edge are gated by
`expandDefinitionContext`, not by a context enum equality check.

### Rewritten `applyRenameTransformer`

```haskell
applyRenameTransformer id stx = withIntroScope \introScope ->
  let introId = identifierScope Nothing introScope id
   in pure (syntaxTrackOrigin [syntax| ?introId:id |] stx)
```

### Rewritten `expandQuoteSyntax`

```haskell
expandQuoteSyntax expr = do
  guardExpressionContext [syntax| (quote-syntax ?expr) |]
  intros <- view expandIntroScopes      -- view, not use (it's now in Reader)
  ph     <- view expandCurrentPhase
  pure [syntax| (quote-syntax ?(syntaxPrune ph intros expr)) |]
```

Intros now reflects only the *current* macro stack, so the symmetry
holds: anything in the result that was added by *this* macro (or an
outer one currently expanding) gets pruned; nothing else is touched.

### Rewritten `partialExpandModuleBegin` define branches

```haskell
TfmCore CoreDefine -> do
  Define id rhs <- matchDefine body
  uscps <- useUseSiteScopes               -- helper that errors out if no ctx
  let usageId = identifierPrune (Phase 0) uscps id
  …
```

where

```haskell
useUseSiteScopes :: Expand ScopeSet
useUseSiteScopes =
  view expandDefinitionContext >>= \case
    Nothing -> error "partialExpandModuleBegin invariant: no DefinitionContext"
    Just dc -> liftIO (readIORef (defctx_use_site_scopes dc))
```

`partialExpandModuleBegin` runs inside `withModuleBeginContext`, which
sets `expand_definition_context = Just _`, so the `error` path is
unreachable in practice — but having a meaningful crash message beats
`fromJust`-on-`Nothing` if the invariant ever drifts.

### `expanderEval` / `EvalState` integration

This is the one ripple I'm uncertain about until I read the evaluator
more carefully. `expanderEval` currently builds an `EvalState`
containing intro and usage scopes:

```haskell
useEvalState = EvalState
  <$> use expandBindingStore
  <*> use expandIntroScopes
  <*> use expandUsageScopes
```

If the evaluator actually consumes these (e.g., for
`syntax-local-introduce`-style operations), then we need to thread
them through differently — either snapshot from `view
expandIntroScopes` (Reader) + `readIORef` the definition context, or
split `EvalState` similarly. If the evaluator just passes them around
without inspecting, we can drop them entirely.

**Open question to resolve during implementation**: read
`Opal.Evaluator` to see whether `eval_intro_scopes` /
`eval_usage_scopes` are read. If not, drop. If so, design the bridge.
Don't pre-decide — let the read drive it.

## Files touched

| File | Change |
|---|---|
| `packages/opal/src/Opal/Expander/DefinitionContext.hs` | **NEW** — defines `DefinitionContext` |
| `packages/opal/src/Opal/Expander/Config.hs` | Add `expand_definition_context :: Maybe DefinitionContext` and `expand_intro_scopes :: ScopeSet`; add lenses; update `defaultExpandConfig` |
| `packages/opal/src/Opal/Expander/State.hs` | **Remove** `expand_intro_scopes` and `expand_usage_scopes` fields and lenses; update `defaultExpandState` |
| `packages/opal/src/Opal/Expander/Monad.hs` | Re-export `expandDefinitionContext`/`expandIntroScopes` lenses; rewrite `withDefinitionContext`/`withModuleBeginContext`/`withTopLevelContext` to allocate `DefinitionContext`; rewrite `withExpressionContext`/`withModuleContext` to clear it |
| `packages/opal/src/Opal/Expander.hs` | Replace `newIntroScope`/`newUsageScope` with `withIntroScope`/`addUseSiteScope`; replace `maybeCreateUseSiteScope`/`maybeCreateInsideEdgeScope` with `addUseSiteScope`/`addInsideEdgeScope`; rewrite `applyTransformer`/`applyRenameTransformer` to use continuation-style; rewrite `expandQuoteSyntax` to read via `view`; rewrite `partialExpandModuleBegin`'s define branches to read from the definition-context box |
| `packages/opal/src/Opal/Evaluator/State.hs` | **Decide**: drop `eval_intro_scopes`/`eval_usage_scopes` if unused, or rework the bridge |
| `packages/opal/src/Opal/Evaluator.hs` / `Evaluator/Monad.hs` | Mirror changes |
| `packages/opal/src/Opal/Expander.hs:328-335` (`expanderEval`'s `useEvalState`) | Adapt to new state layout |
| `packages/opal/opal.cabal` | Add `Opal.Expander.DefinitionContext` to `exposed-modules` |
| `packages/opal/test/Test/Regression.hs` | Add a regression test group |

## Regression test

The canonical test from the scope-sets paper §4.2 and
`use-site-scope-gate-wrong-context.md`:

```scheme
(define-syntax-rule (m x) (define x 1))
(define y 0)
(m y)
y         ; must resolve to the OUTER y (value 1 after m's mutation? or 0?
          ; depends on language semantics — the test is which binding y
          ; resolves to)
```

This is end-to-end (reader → expander → parser → evaluator), so it'll
exercise the use-site mechanism for real. The unit-level tests will be
cheaper:

1. **Two unrelated macros in `(begin …)` don't leak intro scopes to
   each other.** Build two macro invocations, expand both, check that
   the binding store / produced syntax of the first does *not* contain
   the second's intro scope.

2. **`addInsideEdgeScope` attaches the same scope across multiple
   macro outputs in one context.** Two macro expansions in the same
   `withDefinitionContext`; both outputs should carry the *same*
   inside-edge scope.

3. **`addUseSiteScope` accumulates in the per-context box, not
   globally.** Two macros in *separate* `withDefinitionContext`s;
   pruning binders from context A should not strip scopes from
   context B.

4. **`expandQuoteSyntax` only prunes the current macro stack's intro
   scopes.** Nest a `quote-syntax` inside one macro and another inside
   an unrelated macro; pruning must not touch each other's scopes.

These probably need a small `expandUnitTest` harness — invoking
`runExpand` with a constructed `Syntax` and inspecting the resulting
state/output. Worth ~30 lines of test helper. If that's too much yoga,
drop to a smaller subset that exercises just the `IORef`-based
use-site box at the `DefinitionContext` API layer (test the contract
of the new type, not the end-to-end expansion).

## Sequencing inside the commit

Even though it's one commit, the order of edits matters for keeping
the codebase compiling at each step. Suggested working order:

1. Create `Opal.Expander.DefinitionContext` module (compiles
   standalone).
2. Update `Opal.Expander.Config` (compiles; no callers know about new
   field yet — they use `def`).
3. Update `Opal.Expander.State` (this WILL break callers in
   `Expander.hs` that use the removed lenses — expected; move on).
4. Update `Opal.Expander.Monad` (re-exports, context helpers).
5. Rewrite `Opal.Expander.hs` paths in this order:
   a. `newIntroScope`/`newUsageScope` → `withIntroScope`/
      `addUseSiteScope`/`addInsideEdgeScope`
   b. `applyTransformer`/`applyRenameTransformer` (depend on (a))
   c. `expandQuoteSyntax`
   d. `partialExpandModuleBegin` define branches
   e. `expanderEval`'s `useEvalState`
6. Decide on Evaluator state (read `Opal.Evaluator.State` first; act).
7. Update `opal.cabal` exposed-modules.
8. Build, fix any leftover errors.
9. Write regression tests.
10. Sanity-check each test by reverting individual pieces of the
    refactor.

A natural review gate sits between steps 4 and 5: structural pieces
compile, but no expander logic has changed yet. Worth pausing to
confirm direction before tearing into `Expander.hs`.

## Out of scope

- Lambda/letrec/begin internal binder scopes (`expandLambda`,
  `expandLetRec`, `expandBegin`'s outer-edge/inside-edge wiring) —
  these have their own scope-sets bugs (use `scopeId True` where they
  should use `Nothing`), but they're orthogonal to the intro/use-site
  machinery and not named in any of the three issues.
- Filling in the `CoreBeginForSyntax` stubs — separate WIP per the
  previous commit's note.
- Removing the `scopeId`/`scopeSyntax` `Bool` API (the issues note
  it's a code smell but don't require it; can be done after).

## Open questions to resolve before/during implementation

1. **Evaluator integration of intro/usage scopes**: read
   `Opal.Evaluator.State` and the evaluator to determine if they're
   load-bearing. If yes, design the bridge. If no, drop them from
   `EvalState` too.

2. **What context allocates the `DefinitionContext` at the top
   level**: `runExpandSyntax` currently doesn't enter any
   `withXxxContext` explicitly. Either it needs to wrap its body in
   `withTopLevelContext`, or the initial `ExpandConfig` should ship
   with a pre-allocated `DefinitionContext`. The former is cleaner.

3. **`fromJust` on `expand_definition_context` in
   `partialExpandModuleBegin`**: rather than using `fromJust`,
   introduce a helper `useDefinitionContext :: Expand
   DefinitionContext` (or the more specific `useUseSiteScopes ::
   Expand ScopeSet` shown above) that throws a clear internal-error
   if the invariant breaks. Costs ~5 lines, makes future debugging
   much easier.

## Verification

- `nix-shell --run 'cabal new-build opal'` — clean build.
- `nix-shell --run 'cabal new-test opal'` — all existing tests still
  green (29 tests today), plus the new regression group.
- Manual sanity for each regression test: temporarily revert the
  matching part of the refactor and confirm the test fails for the
  right reason (cross-macro leak, missing use-site, etc.). Same
  workflow as the previous fixes.
- After landing, move all three issue files from `review/issues/open/`
  to `review/issues/closed/`.
