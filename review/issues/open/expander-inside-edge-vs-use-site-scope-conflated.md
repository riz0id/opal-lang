# `maybeCreateInsideEdgeScope` is a verbatim copy of `maybeCreateUseSiteScope`

**Severity:** high (the inside-edge scope and the use-site scope serve
distinct roles in the scope-sets model; collapsing them into the same
operation breaks the "remove use-site, keep inside-edge" symmetry that
defines macro hygiene in definition contexts)

**Location:** `packages/opal/src/Opal/Expander.hs:472-488`

## What the code says

```haskell
applyTransformer t stx = do
  …
  introScope <- newIntroScope
  introStx   <- flipSyntax introScope stx

  -- In a definition context, we need use-site scopes
  usageStx <- maybeCreateUseSiteScope introStx

  transformed <- do … expand the macro …

  -- Flip the introduction scope after the transformer has been applied.
  resultStx <- flipSyntax introScope transformed

  -- In a definition context, we need to add the inside-edge scope to
  -- any expansion result
  postStx <- maybeCreateInsideEdgeScope resultStx
  …
  where
    maybeCreateUseSiteScope :: Syntax -> Expand Syntax
    maybeCreateUseSiteScope s = do
      ctx <- view expandContext
      if ctx == ContextDefinition
        then do
          usageScope <- newUsageScope
          scopeSyntax True usageScope s
        else pure s

    maybeCreateInsideEdgeScope :: Syntax -> Expand Syntax
    maybeCreateInsideEdgeScope s = do
      ctx <- view expandContext
      if ctx == ContextDefinition
        then do
          usageScope <- newUsageScope          -- <- same kind of scope!
          scopeSyntax True usageScope s         -- <- same operation!
        else pure s
```

The two helpers have **identical bodies**. Both call `newUsageScope`
(which inserts into `expandUsageScopes`) and both apply the resulting
scope via `scopeSyntax True`. The only thing distinguishing them is
the name and the comment above the call site.

## What the scope-sets model requires

The two scopes are categorically different:

* **Use-site scope** is added to the macro *input* before the
  transformer runs (`flip-introduction-scopes` is the intro-scope
  layer; the use-site scope is layered *additionally* onto the input).
  It is *removed* from definition-context binders that come out of
  the macro's expansion, so that an identifier introduced by the
  caller as a binder doesn't accidentally bind a use of itself
  introduced by the macro itself. Racket:
  `racket/src/expander/expand/use-site.rkt:remove-use-site-scopes`.

* **Inside-edge scope** is added to the macro *result* (and to every
  binder that subsequently appears in the surrounding internal
  definition context). It is the scope that records "this binder was
  introduced inside this definition context" — it is *not* removed.
  Racket:
  `racket/src/expander/expand/definition-context.rkt:add-intdef-scopes`
  / `racket/src/expander/syntax/scope.rkt`'s
  `internal-definition-context-inside-edge`.

The symmetry that makes hygiene work in internal-definition contexts:

> *Use-site* scopes are added to the macro input and removed from the
> binders of definitions that the macro produces. *Inside-edge*
> scopes are added to the macro result and never removed; they are
> what subsequent definitions look up against.

Collapsing both into "create a fresh `usageScope` and slap it on"
breaks the asymmetry. With Opal's current code:

1. The use-site scope added before the macro runs and the inside-edge
   scope added after are *different fresh scopes* (because each call
   to `newScope` is fresh), so the macro input gets `usc_before` and
   the macro output gets `usc_after`.
2. Both go into `expandUsageScopes` (the global accumulator — see
   [[expander-intro-and-use-site-scopes-leak-globally]]).
3. When `partialExpandModuleBegin` prunes use-site scopes from a
   `define`'s binder, it prunes **both** `usc_before` and `usc_after`
   from the binder, removing the inside-edge scope that should have
   stayed.

Net effect: bindings from the macro's expansion become detached from
the surrounding internal-definition scope, so subsequent uses in the
same `begin`-body can't see them.

## Cross-reference

This issue is the placeholder called out from
[[expander-intro-and-use-site-scopes-are-phase-specific]]'s closing
"Cross-reference" section. The intro-and-use-site-globally issue
([[expander-intro-and-use-site-scopes-leak-globally]]) and the
phase-specificity issue
([[intro-and-use-site-scopes-are-phase-specific]]) need to be fixed
together with this one; in isolation, swapping `newUsageScope` for a
distinct `newInsideEdgeScope` accumulator just moves the problem,
because the global accumulators still don't track which scope
belonged to which macro invocation.

## What should happen

Three correlated changes:

1. **Introduce a separate kind of scope for inside-edge.** Racket
   models the inside-edge scope as a `representative-scope` belonging
   to a *multi-scope* (one per definition context); it is shared
   across all macro outputs in that definition context, not freshly
   created per macro call. Opal's nearest analog would be a
   per-definition-context scope created once on entry to the context
   and added to every macro-output that lands in it.

2. **Stop using `newUsageScope` for inside-edge.** Once
   definition-context bookkeeping is in place (see the suggested fix
   in
   [[expander-intro-and-use-site-scopes-leak-globally]]), the
   inside-edge scope can be looked up from the current
   definition-context record rather than freshly minted, and it
   should *not* be added to `expandUsageScopes` at all (so that
   pruning use-site scopes off a binder doesn't strip it).

3. **Apply the inside-edge scope phase-independently.** Like
   use-site/intro scopes, the inside-edge scope should be added with
   phase `Nothing`, not `Just ph` — see
   [[intro-and-use-site-scopes-are-phase-specific]].

Sketch (assuming a definition-context record `DefinitionContext` with
fields `defctx_use_site_scopes :: IORef ScopeSet` and
`defctx_inside_edge_scope :: Scope`):

```haskell
maybeCreateUseSiteScope :: Syntax -> Expand Syntax
maybeCreateUseSiteScope s = do
  view expandDefinitionContext >>= \case
    Nothing  -> pure s
    Just dc  -> do
      usc <- newScope
      liftIO (modifyIORef' (defctx_use_site_scopes dc) (ScopeSet.insert usc))
      pure (syntaxScope Nothing usc s)

addInsideEdgeScope :: Syntax -> Expand Syntax
addInsideEdgeScope s =
  view expandDefinitionContext >>= \case
    Nothing -> pure s
    Just dc -> pure (syntaxScope Nothing (defctx_inside_edge_scope dc) s)
```

…and `applyTransformer` calls `maybeCreateUseSiteScope` before the
transformer and `addInsideEdgeScope` after — the latter unconditional
(no fresh scope, just attach the context's existing one).

## Aside

The duplicated body is also a clear code-smell signal: if the only
difference between two helpers is their names, one of them is wrong.
A short comment-only check during code review would have caught this
("are these supposed to do the same thing? if so, delete one; if not,
they should differ"). Worth a glance whenever a helper is copy-pasted
in this codebase.
