# `expandLetRec` emits the unscoped binder identifiers in its output

**Severity:** medium (the output `letrec` form's binder identifiers
are missing the letrec scope, while their bodies have it. Resolution
against those binders will fail or mis-resolve once the output is
re-walked. Asymmetric with `expandLambda`, which emits the scoped
binders.)

**Location:** `packages/opal/src/Opal/Expander.hs:593-620`

## What the code says

```haskell
valBinds <- for valExprs \(valId, valExpr) -> do
  valId'   <- scopeId True sc valId
  valExpr' <- scopeSyntax True sc valExpr
  binder   <- newBinding valId'
  pure (valId, valExpr', binder)             -- ← returns `valId` (unscoped)
                                             --   not `valId'` (scoped)
```

The tuple stored in `valBinds` has `valId` (the *input* identifier,
without the fresh letrec scope `sc`) for its first component. Later:

```haskell
let letBinds = map (\(idt, _, b) -> (b, idt)) valBinds
                  -- idt is the unscoped valId

withVarTransformers letBinds do
  …
    vals <- for valBinds \(valId, valExpr, _) -> do
      result <- expand valExpr
      pure [syntax| (?valId:id ?result) |]   -- ← still unscoped
  …
    pure [syntax| (letrec (?vals ...) ?result) |]
```

Two uses, both with the unscoped `valId`:

1. **`letBinds`** maps binder symbols to the *unscoped* `valId`,
   which is then used in the environment by `withVarTransformers`.
   For references in the body, `lookupEnvironment` returns the
   unscoped identifier as the transformer payload. This may or may
   not matter for subsequent expansion depending on what consumes the
   payload, but it diverges from `expandLambda`'s `withVarTransformers
   bindings` (which uses scoped `id'`).

2. **Output `letrec` form**'s binder list `(?valId:id ?result)` uses
   the unscoped `valId`. The corresponding `?result` is the *expanded*
   body, which *does* carry `sc` (because `expandLambda`/`scopeSyntax
   True sc expr` was applied to `valExpr` at line 595). So the output
   `(letrec ((x_unscoped <body-with-sc>)) …)` has a mismatch: the
   binder doesn't have `sc`, the body does. Downstream parsing
   (`Opal.Parser`) or subsequent re-expansion that needs to look up
   `x` against the letrec binder will fail to match scope sets.

Compare to `expandLambda` (`Expander.hs:561-570`), which is correct:

```haskell
bindings <- for ids \id -> do
  id'  <- scopeId True sc id
  bind <- newBinding id'
  pure (bind, id')                          -- ← scoped

withVarTransformers bindings do
  let args = map snd bindings                -- ← scoped
  stx'   <- scopeSyntax True sc expr
  result <- expand stx'
  pure [syntax| (lambda (?args:id ...) ?result) |]
                                             -- ← scoped binders in output
```

Lambda uses `id'` (scoped) everywhere; letrec uses `valId` (unscoped)
in two places. The asymmetry is almost certainly an oversight.

## What Racket does

In Racket's `expand` for letrec
(`racket/src/expander/expand/expr.rkt`'s `letrec-syntaxes+values` and
`letrec-values` handlers), the binder identifiers in the expanded
output carry the letrec scope — exactly like the body carries it.
The expander builds the output form by `datum->syntax`-ing the binder
list against the (scoped) context, not by carrying the original
unscoped identifiers through.

## Concrete failure path

```scheme
(let ([y 10])
  (let-values (([x] y))
    x))
```

After Opal expansion (which lowers `let-values` to `letrec-values` or
similar), the inner `x` binder lacks the let-scope while the body
`x` reference has it. Resolution of `x` against `restrictBindings`
fails because the binder's recorded scope set (`{outer-scopes}`)
isn't a subset of the reference's scope set (`{outer-scopes, sc}`) —
well, actually it might pass (binder ⊆ reference is the rule), but
the *binder* recorded in the binding store via `newBinding valId'`
*does* have `sc` (because `valId'` was scoped). So:

* Binding store has: symbol=`x`, scope-set=`{outer, sc}`, binder=gensym.
* The output `letrec`'s binder syntax `valId` has scope-set
  `{outer}` (no `sc`).
* Body's `x` reference has scope-set `{outer, sc}`.

Resolution of body's `x` against `{outer, sc}` finds binding
`{outer, sc} → gensym` and resolves correctly. **At phase 0 the bug
is invisible** because the binding store lookup uses the recorded
scope set, not the syntax-object binder. The output `valId`'s scope
set only matters when the output is re-walked (which Opal's pipeline
currently doesn't do for the same module).

But: if the output is `quote-syntax`'d and then later resolved at a
different phase, or fed back into the expander for a partial
re-expansion, the unscoped binder loses information.

## Suggested fix

Three changes in `expandLetRec`, all replacing `valId` (unscoped) with
`valId'` (scoped):

```haskell
  valBinds <- for valExprs \(valId, valExpr) -> do
    valId'   <- scopeId True sc valId
    valExpr' <- scopeSyntax True sc valExpr
    binder   <- newBinding valId'
-   pure (valId, valExpr', binder)
+   pure (valId', valExpr', binder)
```

(That single change cascades correctly — `letBinds` and the output
`vals` list now both use `valId'`.)

Optional follow-up: do the same audit for `transBinds`. Currently
that tuple uses `binder, value` only, so the scoped/unscoped question
doesn't apply there — but if a future change adds the transformer's
binder identifier to the output (which Racket does for
`letrec-syntaxes+values`'s syntax-bindings group), it should also be
scoped.

## Cross-reference

This issue compounds with
[[lambda-letrec-begin-binder-scopes-phase-specific]]: that issue
recommends switching `scopeId True` / `scopeSyntax True` to phase-
independent (`Nothing`) attachment. After both fixes,
`expandLetRec`'s flow is:

```haskell
valBinds <- for valExprs \(valId, valExpr) -> do
  let valId'   = identifierScope Nothing sc valId
  let valExpr' = syntaxScope     Nothing sc valExpr
  binder <- newBinding valId'
  pure (valId', valExpr', binder)
```

— clean, symmetric with the corrected lambda, no `True`/`False` Bool
parameter, no scoped/unscoped split.
