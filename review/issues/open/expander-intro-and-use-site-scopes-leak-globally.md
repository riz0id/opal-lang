# Intro and use-site scope sets accumulate globally across the entire expansion

**Severity:** high (breaks hygiene across unrelated macro uses; will cause
silent miscompilation and spurious unbound errors once `quote-syntax` and
`define-syntax` are exercised together)

**Locations:**

* `packages/opal/src/Opal/Expander/State.hs:59-62` — fields
  `expand_intro_scopes :: ScopeSet` and `expand_usage_scopes :: ScopeSet`
  on `ExpandState`.
* `packages/opal/src/Opal/Expander.hs:255-268` — `newIntroScope` /
  `newUsageScope` *insert* into the state set, never remove.
* `packages/opal/src/Opal/Expander.hs:610-618` — `expandQuoteSyntax` reads
  the running set and prunes everything in it.
* `packages/opal/src/Opal/Expander.hs:736-740, 748-752` —
  `partialExpandModuleBegin`'s `define` and `define-syntax` branches read
  the running use-site set and prune *everything in it* off the binder.

## What the code does

```haskell
newIntroScope :: Expand Scope
newIntroScope = do
  sc <- newScope
  expandIntroScopes %= ScopeSet.insert sc
  pure sc

newUsageScope :: Expand Scope
newUsageScope = do
  sc <- newScope
  expandUsageScopes %= ScopeSet.insert sc
  pure sc
```

These are the only writers. Nothing ever calls `expandIntroScopes %=
ScopeSet.delete _` or restores a previous value via `local`. The two
`ScopeSet`s are monotonically growing for the lifetime of the expansion.

Then `expandQuoteSyntax`:

```haskell
expandQuoteSyntax expr = do
  guardExpressionContext [syntax| (quote-syntax ?expr) |]
  phase  <- view expandCurrentPhase
  intros <- use expandIntroScopes
  let result = syntaxPrune phase intros expr
  pure [syntax| (quote-syntax ?result) |]
```

`syntaxPrune phase intros expr` strips every scope in `intros` from `expr`
at `phase`. Since `intros` contains every intro scope from every macro
ever invoked, this prunes scopes from macros that are *not* on the path
of this `quote-syntax`.

And `partialExpandModuleBegin`:

```haskell
TfmCore CoreDefine -> do
  Define id rhs <- matchDefine body
  uscps <- use expandUsageScopes
  phase <- view expandCurrentPhase
  let usageId = identifierPrune phase uscps id   -- strips ALL accumulated use-site scopes
  binder <- newBinding usageId
  ...
```

…same pattern: every use-site scope ever created in the module is
removed from this binder, regardless of which macro call's
definition-context it actually appeared in.

## What Racket does

Racket scopes both stacks *contextually*, not as monotonic globals.

* **Intro scopes** live on the dynamic `expand-context` (see
  `racket/src/expander/expand/context.rkt`, fields
  `current-introduction-scopes` and `current-use-scopes`). Around each
  macro expansion, `flip-introduction-scopes` symmetrically applies *only
  the current macro's intro scopes* — the prior macro's intro scopes
  are not visible inside the inner expansion because the context is
  re-bound via dynamic-extent helpers
  (`racket/src/expander/expand/main.rkt:apply-transformer` parameterizes
  `current-expand-context` before recursing).

* **Use-site scopes** are stored in a box-per-definition-context:
  `(internal-definition-context-use-site-scopes ctx)` is a `(box list)`.
  When a binder needs them pruned, only that context's box is consulted
  (`remove-intdef-use-site-scopes` /
  `remove-use-site-scopes` in
  `racket/src/expander/expand/use-site.rkt`). Two unrelated macro uses
  in the same module body each push their own use-site scopes into the
  enclosing context's box, and only binders coming out of *that* macro
  expansion get those scopes removed.

The scope-sets paper formalizes this in §4.2 ("Use-Site Scopes"): the
use-site scope is created *at the macro invocation* and removed *only
from the binders that propagate up through the same macro*.

## Concrete failure modes

1. **Cross-macro intro leak.** Suppose macros `M1` and `M2` are both
   used in a `(begin ...)` body. `M1` creates intro scope `i1`, `M2`
   creates `i2`. Each expansion flips its own intro scope on input/
   output. After both have run, `expandIntroScopes = {i1, i2}`. A
   `quote-syntax` inside the result of `M2` then prunes both `i1` and
   `i2` from its expression — but the expression never carried `i1` to
   begin with, so this is "harmless"… unless `M1`'s expansion result
   passed through `M2`'s body, in which case `i1` is part of the legitimate
   scope set of that piece of syntax. Pruning it strips a real hygiene
   mark and changes which binding the identifier resolves to.

2. **Cross-macro use-site leak in module-begin.** Two macro uses in a
   module body each produce a `(define x ...)` form. Each macro
   contributed a use-site scope (`u1` and `u2`). When the first
   `define` is processed, `expandUsageScopes = {u1, u2}` (both have
   been created by the time we reach the second pass), so the binder
   for `x` from macro `M1` gets `u2` pruned off it — a scope it
   shouldn't have had pruned, since `u2` was never part of `M1`'s
   expansion path. This can cause the binder's scope set to *lose*
   scopes legitimately attached by an outer context, breaking
   resolution of references inside the macro's body.

3. **`quote-syntax` returns mis-scoped syntax**. The scope-pruning
   step on `quote-syntax` is supposed to mirror the macro-intro flip:
   anything that survives the symmetry was *not* added by the current
   macro, and should be preserved verbatim. Pruning *all historical*
   intro scopes guarantees that any nested macro use loses its outer
   macro's intro scope — flipping behavior breaks symmetry, and
   identifiers that should still be marked as intro'd by `M1` come out
   un-intro'd.

## Suggested fix

Move the intro/use-site sets out of `ExpandState` and into
`ExpandConfig` (or into a `Reader`-shaped helper), then restore the
prior value on exit using `local`:

```haskell
-- Replacing `newIntroScope`'s monotonic insert:
withIntroScope :: (Scope -> Expand a) -> Expand a
withIntroScope k = do
  sc   <- newScope
  prev <- view expandIntroScopes
  local (expandIntroScopes %~ ScopeSet.insert sc) (k sc)
```

…and migrate `applyTransformer`, `applyRenameTransformer`, and
`maybeCreateUseSiteScope` to take a continuation, so the scope is
visible only inside the macro's recursive expansion.

For the module-begin use-site pruning, attach the use-site scope set
to the *definition context* (mirror Racket's `internal-definition-
context-use-site-scopes`): each `withDefinitionContext` allocates a
fresh box of use-site scopes, and only that box's contents are pruned
on its `define`/`define-syntax` binders.

Concretely:

```haskell
data DefinitionContext = DefinitionContext
  { defctx_use_site_scopes :: IORef ScopeSet }

withDefinitionContext :: Expand a -> Expand a
withDefinitionContext k = do
  box <- liftIO (newIORef ScopeSet.empty)
  local (set expandDefinitionContext (Just (DefinitionContext box))) k
```

…and `partialExpandModuleBegin`'s define branches read from `box`
instead of the global accumulator.

## Cross-reference

This is the same root cause as
[[intro-and-use-site-scopes-are-phase-specific]] (the scopes are
attached at the *current phase* instead of phase-independently); both
issues need to be fixed together to make intro/use-site hygiene work
correctly.
