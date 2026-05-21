# Macro-introduction and use-site scopes are attached as phase-specific scopes

**Severity:** high (breaks hygiene at non-base phases; will misbehave the
moment Opal exercises `define-syntax` / `begin-for-syntax` at all
seriously)

**Locations:**

* `packages/opal/src/Opal/Expander.hs:291-294` — `flipSyntax`
* `packages/opal/src/Opal/Expander.hs:441-442, 461-462` — macro intro scope
  flips in `applyTransformer`
* `packages/opal/src/Opal/Expander.hs:472-488` — `maybeCreateUseSiteScope`
  and `maybeCreateInsideEdgeScope`
* `packages/opal/src/Opal/Syntax.hs:571-581` — `syntaxFlipScope`
* `packages/opal/src/Opal/Syntax/ScopeInfo.hs:141-142` — `ScopeInfo.flipScope`

## What Opal does

The expander helper is:

```haskell
flipSyntax :: Scope -> Syntax -> Expand Syntax
flipSyntax sc id = do
  ph <- view expandCurrentPhase
  pure (syntaxFlipScope ph sc id)
```

`syntaxFlipScope :: Phase -> Scope -> Syntax -> Syntax` calls
`ScopeInfo.flipScope`, which only manipulates the per-phase
`MultiScope` part of `ScopeInfo`:

```haskell
flipScope :: Phase -> Scope -> ScopeInfo -> ScopeInfo
flipScope ph sc (ScopeInfo gscps mscps) = ScopeInfo gscps (MultiScope.flipScope ph sc mscps)
```

There is no `flipScope`/`syntaxFlipScope` overload that operates on the
*phase-independent* (`gscps`) part of `ScopeInfo`. The expander never
calls one even if it existed.

In `applyTransformer`, the macro-introduction scope is created and
flipped:

```haskell
introScope <- newIntroScope
introStx   <- flipSyntax introScope stx
...
resultStx <- flipSyntax introScope transformed
```

…so the intro scope only ever appears in the per-phase scope map at the
expander's current phase. Same story for the use-site scope:

```haskell
maybeCreateUseSiteScope s = do
  ...
  scopeSyntax True usageScope s  -- True = phase-specific
```

where `scopeSyntax True` calls `syntaxScope (Just ph) sc id`.

## What Racket does

In Racket's expander, the macro introduction scope and the use-site
scope are *regular* scopes, attached phase-independently. Concretely
(`racket/src/expander/expand/definition-context.rkt:391`):

```scheme
(define (flip-introduction-scopes s ctx)
  (flip-scopes s (expand-context-current-introduction-scopes ctx)))
```

…where `flip-scopes` is the phase-independent
`racket/src/expander/syntax/scope.rkt:578`:

```scheme
(define (flip-scope s sc)
  (apply-scope s (generalize-scope sc) set-flip propagation-flip))
```

`generalize-scope` only routes through the per-phase store when `sc` is
a `representative-scope` (the per-phase scope owned by a *multi-scope*,
e.g. a module's inside-edge scope). Macro-intro scopes and use-site
scopes are plain `scope?` values — they live in the phase-independent
scope set and are visible at every phase.

This is what the scope-sets paper requires too: in §4 "Macros" and
§4.2 "Use-Site Scopes", the introduction scope is added to the
identifier "regardless of phase" so that the symmetry argument
(removing the same scope after expansion) is well-defined when the
expanded form is later visited at a different phase.

## Why this matters

Two concrete failure modes:

1. **`define-syntax` followed by a phase-0 use of the syntax inside a
   `begin-for-syntax` block (i.e. a phase-1 use).** When the macro is
   used at phase 1, Opal flips the intro scope at phase 1 only. If the
   macro body returns a piece of syntax that is itself a use of another
   macro at phase 0, the second expansion runs at phase 0 — but the
   intro scope is sitting in the phase-1 slot. The "unflip after
   expansion" step at phase 1 then correctly removes it from phase 1,
   but any phase-0 inspection during expansion (e.g. resolving an
   identifier that was decorated with the intro scope at phase 1) sees
   the wrong scope set.

2. **Use-site scopes pruned at definition.** Racket's
   `remove-use-site-scopes` (`expand/use-site.rkt:12`) removes the
   accumulated use-site scopes from the *identifier being bound* at any
   phase. Opal stores use-site scopes per-phase in the multi-scope,
   meaning the binder retains the use-site scope at every phase except
   the current one. A subsequent reference at a different phase will
   then *not* see the use-site scope as "removed" and resolution will
   incorrectly include the use-site scope in the candidate set.

The failure is silent: at the base phase 0 with no `begin-for-syntax`,
the bug is invisible because both add and remove happen at phase 0, so
the symmetry holds within a single phase. The first non-trivial
multi-phase test will surface it.

## Suggested fix

Two pieces:

1. Generalize `syntaxFlipScope` / `syntaxScope`'s phase parameter to
   `Maybe Phase`, mirroring `syntaxScope`'s existing signature. Pass
   `Nothing` for intro and use-site scopes. Add a corresponding
   `ScopeInfo.flipScope :: Maybe Phase -> Scope -> ScopeInfo ->
   ScopeInfo` that flips on `gscps` when the phase is `Nothing`.

2. Change `flipSyntax`, `applyTransformer`'s `maybeCreateUseSiteScope`,
   and the inside-edge scope helper to use `Nothing` for the phase
   argument:

```haskell
flipSyntax :: Scope -> Syntax -> Expand Syntax
flipSyntax sc = pure . syntaxFlipScope Nothing sc

maybeCreateUseSiteScope s = do
  ctx <- view expandContext
  if ctx == ContextDefinition
    then do
      usageScope <- newUsageScope
      pure (syntaxScope Nothing usageScope s)
    else pure s
```

Note that `scopeId`'s `Bool` parameter (`True = phase-specific, False =
phase-independent`) is also a code smell — switch the API to
`Maybe Phase` everywhere and let callers be explicit. Module
inside-edge scopes (the only thing that *should* be phase-specific)
would be `Just ph`; intro/use-site/macro-intro scopes would be
`Nothing`.

## Cross-reference

`maybeCreateInsideEdgeScope` is a duplicated copy of
`maybeCreateUseSiteScope` (same body). At the very least one of them
should be deleted or one should differ; in Racket they are genuinely
different scopes — see `intdef-add-scopes` /
`internal-definition-context-{outside,inside}-edge` —
[[expander-inside-edge-vs-use-site-scope-conflated]].
