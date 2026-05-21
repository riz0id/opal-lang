# `partialExpandModuleBegin` did not iterate on macro outputs

**Severity:** high (any macro use in a module body that produced a
`(define …)` or `(define-syntax …)` form left it inert — the form
flowed through the rest of the pipeline as unprocessed syntax,
never reaching `newBinding`, never landing in the module's
namespace. The signature failure mode for macros that introduce
bindings, which is most of them in any non-trivial language.)

**Location:** `packages/opal/src/Opal/Expander.hs` —
`partialExpandModuleBegin`'s `_` fallthrough for non-Core
transformers.

## What the code said

```haskell
partialExpandModuleBegin = loop
  where
    loop (body : bodies) = case body of
      stx@[syntax| (?f:id ?args ...) |] -> do
        transformer <- lookupEnvironment f
        case transformer of
          TfmCore CoreBegin       -> …
          TfmCore CoreDefine      -> …    -- registers binder
          TfmCore CoreDefineSyntax -> …   -- registers transformer
          TfmCore CoreImport      -> …
          TfmCore CoreExport      -> …
          TfmCore CoreModule      -> …
          _ -> do
            -- Save for next module body expansion pass.
            bodies' <- loop bodies
            pure (body : bodies')         -- ← passes macro use through verbatim
```

The `_` fallthrough — which covers `TfmDatum` (user-defined macros) —
preserved the macro call verbatim. No subsequent pass actually applied
the macro at module-body level: `expandModuleBeginExprs` only matches
`(define …)`, and `expandModuleExports` calls `expand` (which
*does* dispatch the macro, but at expression level — its output
becomes inert body syntax, not a registered binding).

So for an example like:

```scheme
(define-syntax make-defn (lambda (stx) (quote-syntax (define x 1))))
(make-defn y)
```

…the macro call `(make-defn y)` was preserved through
`partialExpandModuleBegin`. `expandModuleExports` later expanded it
into `(define x 1)`, but that `(define x 1)` was *just syntax in the
final body* — no `newBinding` for `x`, no entry in
`expand_namespace.defns_variables`, no module-level binding produced.

The example file's final namespace would show only `y` defined, not
`x`. (And, even worse, after the next bug fix, `(define x 1)` would
*flow* through `expandModuleExports` and get re-expanded into a
bad-syntax error.)

## What Racket does

Racket's `expand-module` iterates over the module body. When a form
expands to a macro use, the expander recursively partial-expands the
result before continuing — so a macro that produces a `(define …)`
gets its output fed back through the same definition-recognition
pass. The relevant code is the `partial-expand` loop in
`racket/src/expander/expand/module.rkt`, which keeps applying
transformers until the head of each body form is a recognised core
form.

## Fix

Add an explicit `TfmDatum` branch to `partialExpandModuleBegin` that
dispatches the macro and feeds the result back into `loop`:

```haskell
TfmDatum _ -> do
  -- A macro use at the module-body level. Apply the transformer
  -- and re-feed its output through this same loop so that, if it
  -- produced a `(define …)` or `(define-syntax …)` form, it gets
  -- registered as a binding.
  expanded <- dispatch transformer stx
  loop (expanded : bodies)
```

`dispatch transformer stx` reuses the existing dispatch path
(`dispatchTransformer (DatumLam fun) stx = applyTransformer fun stx`
for lambda transformers), which is what `expand` would have called
anyway — but now the output goes through the partial-expand pass that
*recognises* defines.

The recursion is bounded by the macro returning a non-macro form
(otherwise it's an infinite macro-expansion loop, which the user's
macro definition would be responsible for, not the expander).

## Discovered

While running `examples/macro-hygiene.opal`. The previous three fixes
got `(define-syntax make-defn …)` through the pipeline; this one was
needed to make `(make-defn y)`'s output `(define x 1)` actually
register `x` as a binding.

## Cross-reference

All four fixes (`define-syntax-rhs-not-in-expression-context`,
`define-syntax-transformer-not-in-environment`,
`module-exports-pass-re-expands-define-forms`, and this one) were
found in sequence by trying to actually run `examples/macro-hygiene.opal`
end-to-end. The lesson is that the expander pipeline had not been
exercised by any test that ran a real `define-syntax`/`define`
chain — closing this gap with an integration test that loads an
example file (or a synthetic equivalent) would surface regressions
in this family of bugs without requiring four separate single-issue
discoveries.
