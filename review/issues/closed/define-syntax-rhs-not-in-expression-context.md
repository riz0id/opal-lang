# `define-syntax`'s transformer RHS was expanded in the wrong context

**Severity:** high (every `define-syntax` form whose transformer body
contains a `lambda` — i.e. every transformer that isn't a trivial
literal — hit `ErrorBadContext` because `expandLambda` rejected
`ContextModuleBegin`)

**Location:** `packages/opal/src/Opal/Expander.hs` —
`partialExpandModuleBegin`'s `CoreDefineSyntax` branch (around
line 789).

## What the code said

```haskell
TfmCore CoreDefineSyntax -> do
  …
  expr <- nextPhase do
    sexp  <- expandAndParseSyntax rhs    -- ← `rhs` walks under
    value <- expanderEval sexp           --   the surrounding
    pure (binder, value)                 --   ContextModuleBegin
```

`partialExpandModuleBegin` runs inside `withModuleBeginContext`. The
transformer body is then expanded with that context still active —
`expand`ing the body's `lambda` form invokes
`expandLambda`'s `guardExpressionContext`, which rejects
`ContextModuleBegin` and produces:

```
invalid expansion context
  * expanding the syntax object:
    #'('lambda '('stx) '('quote-syntax '('define 'x 1)))
    * can only be expanded in a [expression,definition] context
    * but was expanded in a module-begin context
```

So any source with a non-trivial `define-syntax` failed to expand.

## What Racket does

Racket's `expand-defined-syntax` (`racket/src/expander/expand/module.rkt`)
switches to an expression context when walking the transformer's
right-hand side. The transformer body is an expression — it produces
a compile-time value. Its lexical environment for context-sensitive
checks is *expression*, not *module-begin*.

Compare to the parallel `CoreDefine` handling in
`expandModuleBeginExprs` (`Expander.hs`), which already does the
right thing:

```haskell
[syntax| (define ?id:id ?rhs) |] -> do
  ph   <- view expandCurrentPhase
  expr <- withExpressionContext (expand rhs)   -- ← correct
  …
```

The `CoreDefineSyntax` branch was missing the same wrap.

## Fix

Wrap the `nextPhase` block in `withExpressionContext`:

```haskell
expr <- nextPhase $ withExpressionContext do
  sexp  <- expandAndParseSyntax rhs
  value <- expanderEval sexp
  pure (binder, value)
```

The `nextPhase`/`withExpressionContext` ordering matters here:
`nextPhase` is the dynamic-phase shift; the expression context applies
*inside* the next-phase action.

## Discovered

While running `examples/macro-hygiene.opal` end-to-end for the first
time. The example exercises `define-syntax` with a `lambda`
transformer body and hit this immediately.
