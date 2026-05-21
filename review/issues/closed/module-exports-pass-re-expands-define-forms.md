# `expandModuleExports` re-expanded already-processed `define` / `define-syntax` forms

**Severity:** high (after `partialExpandModuleBegin` correctly
processed and lifted `define-syntax` forms, the subsequent
`expandModuleExports` pass walked the module body again and called
`expand` on each non-export form — which routes back through
`dispatchCoreForm CoreDefineSyntax` → `throwBadSyntax`)

**Location:** `packages/opal/src/Opal/Expander.hs` —
`expandModuleExports`'s `loop` fallthrough.

## What the code said

```haskell
expandModuleExports original = do
  (specs, result) <- loop [] original
  pure (Export 0 specs, result)
  where
    loop specs (body : bodies) = case body of
      [syntax| (export ?_ ...) |] -> …      -- only exports special-cased
      [syntax| ?stx |] -> do
        body' <- expand stx                  -- ← everything else gets re-expanded
        …
```

By the time we reach `expandModuleExports`, the module body has been
through `partialExpandModuleBegin` (which processed all
`define-syntax`/`define`/`import`/`export` forms) and
`expandModuleBeginExprs` (which expanded `define` RHSs). The remaining
forms are *already-processed* `define`/`define-syntax`/`export` plus
raw expressions.

The fallthrough `[syntax| ?stx |] -> expand stx` re-expanded
EVERYTHING that wasn't an export — including the already-processed
define-syntax forms. `expand` on `(define-syntax …)` routes to
`dispatchCoreForm CoreDefineSyntax`, which is `throwBadSyntax
CoreDefineSyntax`. So:

```
bad 'define-syntax syntax
  #'('define-syntax 'make-defn (lambda (g14) #'('define 'x 1)))
```

Same bug would have applied to `define` if any test had exercised
it, since `dispatchCoreForm CoreDefine` also ends with
`pure (defineToSyntax (Define id rhs))` — which would walk the rhs
through `expand` again, producing wrong scope-set arithmetic.

## What Racket does

Racket's module-finalize pass walks the body to *collect* exports
and to fully expand any leftover expression forms. It does not
re-expand definitions; those are already in `expand-state-bindings`
and the body form is preserved verbatim. See
`racket/src/expander/expand/module.rkt`'s post-loop walk.

## Fix

Pattern-match on `define` and `define-syntax` before the catch-all
`?stx`, and pass them through unchanged:

```haskell
loop specs (body : bodies) = case body of
  [syntax| (export ?_ ...) |] -> …
  [syntax| (define        ?_dId  ?_dRhs)  |] -> do
    (specs', bodies') <- loop specs bodies
    pure (specs', body : bodies')
  [syntax| (define-syntax ?_dsId ?_dsRhs) |] -> do
    (specs', bodies') <- loop specs bodies
    pure (specs', body : bodies')
  [syntax| ?stx |] -> do
    body' <- expand stx
    …
```

The fresh meta-variable names (`?_dId`, `?_dRhs`, etc.) are required
by the quasiquoter — it forbids reusing the same `_` in two pattern
slots.

## Discovered

While running `examples/macro-hygiene.opal`. The previous two fixes
got `(define-syntax …)` through `partialExpandModuleBegin`; the next
pass then re-encountered it.
