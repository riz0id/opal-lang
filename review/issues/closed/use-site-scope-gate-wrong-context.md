# Use-site scopes are only created in `ContextDefinition` but consumed in `ContextModuleBegin`

**Severity:** high (use-site hygiene never engages for top-level module
bodies; the very place Racket's use-site mechanism is most active is
the one place Opal disables it)

**Locations:**

* `packages/opal/src/Opal/Expander.hs:472-479` —
  `maybeCreateUseSiteScope` gating
* `packages/opal/src/Opal/Expander.hs:733-744` —
  `partialExpandModuleBegin`'s `CoreDefine` branch (reader)
* `packages/opal/src/Opal/Expander.hs:745-763` — same, for
  `CoreDefineSyntax`

## What the code does

`applyTransformer` decides whether to attach a use-site scope to the
macro input based purely on the current `expandContext`:

```haskell
maybeCreateUseSiteScope :: Syntax -> Expand Syntax
maybeCreateUseSiteScope s = do
  ctx <- view expandContext
  if ctx == ContextDefinition
    then do
      usageScope <- newUsageScope
      scopeSyntax True usageScope s
    else pure s
```

So a use-site scope is created (and pushed into `expandUsageScopes`)
*only* when the current context is exactly `ContextDefinition` — i.e.
inside an internal `(begin ...)` body that
`preExpandBegin`/`withDefinitionContext` enters
(`Expander.hs:638-647`).

But the place where use-site scopes are *removed* from binders is
`partialExpandModuleBegin`:

```haskell
TfmCore CoreDefine -> do
  Define id rhs <- matchDefine body
  uscps <- use expandUsageScopes
  phase <- view expandCurrentPhase
  let usageId = identifierPrune phase uscps id
  binder <- newBinding usageId
  …
```

`partialExpandModuleBegin` is invoked from `expandModule`'s
`withModuleBeginContext` block (`Expander.hs:691`), which sets the
context to `ContextModuleBegin`. Macros expanded *during* this pass
(via `applyTransformer` recursively, e.g., when a macro use appears in
the module body) see `ctx == ContextModuleBegin`, not
`ContextDefinition`. So `maybeCreateUseSiteScope` returns `s`
unmodified.

The net effect:

* No use-site scope is added to any macro input expanded as a
  top-level module body form.
* But the pruning step at `Expander.hs:736-739` and
  `Expander.hs:748-751` still reads `expandUsageScopes` and prunes
  whatever it finds there (which may be the leftover scopes from
  *some other* nested `begin`-body expansion — see
  [[expander-intro-and-use-site-scopes-leak-globally]]).

So: in module bodies, use-site scopes are never created *for that
context*, but the cleanup at module-define time still runs and can
strip unrelated scopes.

## What Racket does

Racket's macro-call entry point
(`racket/src/expander/expand/main.rkt:expand` → `apply-transformer`)
creates a use-site scope *whenever the current expand-context's
`def-ctx-scopes` parameter contains a definition-context record*. That
parameter is set by every internal-definition context **and** by the
module-body expansion pass:

* `racket/src/expander/expand/module.rkt:expand-module` sets up an
  internal-definition context for the module body
  (`make-module-context` / `expand-context-make-internal-definition-context`)
  before walking the body. Each macro call inside the body therefore
  sees a non-empty `def-ctx-scopes` and a use-site scope is created.

* `racket/src/expander/expand/body.rkt:expand-body` does the same for
  internal `(begin …)` definition contexts.

So in Racket, use-site scopes work the same way at module-top-level as
they do at internal-definition top-level — both are "definition
contexts". Opal's distinction between `ContextModuleBegin` and
`ContextDefinition` does not exist in Racket; both correspond to
"this is an internal definition context."

The scope-sets paper (§4.2 "Use-Site Scopes") motivates this with the
canonical example:

```scheme
(define-syntax-rule (m x) (define x 1))
(m y)
```

The `define-syntax-rule` macro expands `(m y)` into `(define y 1)`,
and the `define`'s `y` must bind to the *outer* `y` — not a copy
introduced through the macro. The use-site scope mechanism is what
enforces that. This example is a *module-body* macro use, not an
internal definition; Racket's expander adds and removes the use-site
scope here. Opal's gate prevents it.

## Concrete failure mode

```
(define-syntax-rule (m x) (define x 1))
(m y)
```

In Racket: `y` resolves to the outer `y` binding. (Use-site scope
flipping cancels out.)

In Opal as written:

1. `(m y)` is processed by `partialExpandModuleBegin` in
   `ContextModuleBegin`.
2. `applyTransformer m (m y)`:
   * `introScope` is created and flipped into the input.
   * `maybeCreateUseSiteScope` sees `ctx == ContextModuleBegin`, does
     **nothing**.
   * The macro produces `(define y 1)`.
   * `introScope` is flipped out.
3. The returned `(define y 1)` is processed by
   `partialExpandModuleBegin`'s `CoreDefine` branch. It reads
   `expandUsageScopes` — which may contain *unrelated* scopes from
   prior nested expansions. It prunes whatever is in that set from
   `y`'s scope set.

So `y` may have *unrelated* scopes pruned (if any other macro use
earlier in the module body, at any nesting depth, created a use-site
scope before this point), corrupting its scope set. Even when no
unrelated scopes are pruned, the *intended* use-site cancellation
(adding a fresh scope to the input that bubbles out via the binder)
never happens, so any case where the canonical use-site idiom matters
(`define-syntax-rule (m x) (define x …) → (m existing-name)`) breaks
in the opposite direction — `existing-name` gets *no* extra scope to
distinguish the macro-introduced binder from the caller's name.

## Suggested fix

The gate should fire for any definition context, including module
bodies. Two ways to spell this:

1. Broaden the context comparison:

   ```haskell
   if ctx `elem` [ContextDefinition, ContextModuleBegin, ContextTopLevel]
     then …
     else pure s
   ```

   …matching what `guardDefinitionContext` already does
   (`Expander.hs:192-198`).

2. Better: track the definition-context record on the reader explicitly
   (a `Maybe DefinitionContext` field on `ExpandConfig`), set it on
   entry to any definition-context pass (internal `begin`, module-begin,
   top-level), and gate `maybeCreateUseSiteScope` on `view
   expandDefinitionContext`. This is the approach Racket takes
   (`expand-context-def-ctx-scopes`) and is the structural fix needed
   to address [[expander-intro-and-use-site-scopes-leak-globally]] as
   well.

The minimal fix is (1); the durable fix is (2), and (2) subsumes the
fixes needed for the two existing cross-referenced issues.
