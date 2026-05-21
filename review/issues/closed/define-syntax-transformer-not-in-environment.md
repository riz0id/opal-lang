# `define-syntax` did not register its transformer in `expandEnvironment`

**Severity:** high (the macro produced by `define-syntax` was
unreachable: identifier resolution returned its binder gensym, but
`lookupEnvironment` could not find the gensym in the env, so every
subsequent macro call errored with "lacks a binding in the
compile-time environment")

**Location:** `packages/opal/src/Opal/Expander.hs` —
`partialExpandModuleBegin`'s `CoreDefineSyntax` branch.

## What the code said

```haskell
TfmCore CoreDefineSyntax -> do
  …
  binder <- newBinding usageId           -- registers in binding store
  expr <- nextPhase $ withExpressionContext do
    sexp  <- expandAndParseSyntax rhs
    value <- expanderEval sexp
    pure (binder, value)

  -- value is in `snd expr`. But it goes into the NAMESPACE only:
  expandNamespace . nsTransformer phase (id ^. idtSymbol) .= Just (TfmDatum (snd expr))
  …
```

The transformer was stored in `expandNamespace.nsTransformer`. But
`Opal.Expander.Monad.lookupEnvironment` consults
`expandEnvironment`, not the namespace:

```haskell
lookupEnvironment id = do
  ph   <- view expandCurrentPhase
  bind <- resolve ph id
  env  <- use expandEnvironment             -- ← env, not namespace
  case Environment.lookup bind env of
    Nothing -> throwError (ExpandNotBound …)
    Just x  -> pure x
```

So when the macro is later called:

1. The identifier `make-defn` resolves to its binder gensym (e.g.
   `g13`).
2. `Environment.lookup g13 env` returns `Nothing` (the transformer
   was put in the namespace, not the env).
3. Error: "the generated symbol 'g13 bound to the identifier
   #'make-defn lacks a binding in the compile-time environment".

Compare to the parallel `CoreDefine` branch:

```haskell
TfmCore CoreDefine -> do
  …
  binder <- newBinding usageId
  expandEnvironment %= Environment.insert binder (TfmDatum (DatumStx (identifierToSyntax usageId)))
  …
```

`CoreDefine` inserts the binder into the env. `CoreDefineSyntax` did
not.

## What Racket does

Racket's `register-defined-syntax` registers the transformer in both
the namespace (for module reflection / `dynamic-require`) and in the
expand-context's environment (for in-module lookup during expansion).
Opal mirrors the namespace half but missed the environment half.

## Fix

Add the missing insert:

```haskell
expandEnvironment %= Environment.insert binder (TfmDatum (snd expr))
expandNamespace . nsTransformer phase (id ^. idtSymbol) .= Just (TfmDatum (snd expr))
```

(Order doesn't matter; they're independent state cells.)

## Discovered

While running `examples/macro-hygiene.opal`. Followed the previous
fix (`define-syntax-rhs-not-in-expression-context`): once
`define-syntax` could evaluate its transformer, the *next* failure
was the macro call not finding the transformer.
