# `EvalConfig.eval_current_scope` is never read

**Severity:** low (cosmetic / cleanup; latent footgun if anyone adds
code that *expects* it to be populated)

**Locations:**

* `packages/opal/src/Opal/Evaluator/Config.hs:50` — field declaration
* `packages/opal/src/Opal/Evaluator/Config.hs:25,56` — re-export
* `packages/opal/src/Opal/Evaluator/Monad.hs:28,51` — re-export
* `packages/opal/src/Opal/Expander.hs:486,346-358` — `expanderEval`
  populates it from `applyTransformer`'s intro scope

## What the code says

```haskell
-- Opal.Evaluator.Config
data EvalConfig = EvalConfig
  { eval_environment   :: Environment
  , eval_current_phase :: {-# UNPACK #-} !Phase
  , eval_current_scope :: Maybe Scope
    -- ^ An optional introduction scope. When given, this scope will
    -- be used for local expansion.
  }
```

```haskell
-- Opal.Expander.applyTransformer
applyTransformer t stx = withIntroScope \introScope -> do
  …
  result <- expanderEval (Just introScope) expr   -- ← passes intro scope here
  …
```

```haskell
-- Opal.Expander.expanderEval
expanderEval sc expr = do
  …
  config <- viewEvalConfig
  …
  where
    viewEvalConfig = do
      env <- use expandEnvironment
      ph  <- view expandCurrentPhase
      pure (EvalConfig env ph sc)     -- ← packed into EvalConfig
```

So `applyTransformer` allocates a fresh intro scope and threads it
through to the evaluator via `eval_current_scope`. The evaluator's
config-comment says it's "used for local expansion."

Grep for any consumer of `evalCurrentScope` or `eval_current_scope`:

```
$ grep -rn "evalCurrentScope\|eval_current_scope" packages/opal/src/
Opal/Evaluator/Monad.hs:28:  , evalCurrentScope     -- re-export only
Opal/Evaluator/Monad.hs:51:  , evalCurrentScope     -- re-export only
Opal/Evaluator/Config.hs:25:  , evalCurrentScope    -- re-export only
Opal/Evaluator/Config.hs:50:  , eval_current_scope :: Maybe Scope
```

**Zero readers.** The field is declared, re-exported, set by
`expanderEval`, and never consulted by any code path in the evaluator
or anywhere else.

## Why this matters (a little)

This is the same shape of bug as the dead `eval_intro_scopes` /
`eval_usage_scopes` fields the macro-state refactor removed. Those
were dead state declared but never read; we dropped them.
`eval_current_scope` survives because it's structurally separate
(declared in `EvalConfig`, not `EvalState`) and wasn't part of the
macro-state surgery.

The field is a *latent footgun*: a future author reading
`applyTransformer`'s `expanderEval (Just introScope) expr` could
reasonably assume the intro scope is in fact used by the evaluator —
e.g. for a `syntax-local-introduce`-style operation — and write code
that relies on it. It isn't. Today the threading is purely
performative.

## What Racket does with the analog

Racket's evaluator has a `current-introduction-scopes` parameter that
*is* read by `syntax-local-introduce` and a few related transformer-
side primitives. Opal doesn't implement those yet (the SExp evaluator
is lambda calculus + variables + literals; there's no `syntax-local-*`
surface). So today the field is genuinely useless.

## Suggested action

Pick one of:

1. **Drop the field.** Remove `eval_current_scope` from `EvalConfig`,
   its lens, its re-exports, and the `Maybe Scope` argument to
   `expanderEval` (it's only ever called with `Just introScope` from
   `applyTransformer` and `Nothing` from two other call sites — all
   uses can be deleted). Net ~10 lines removed.

2. **Plumb it through to the evaluator.** Add a `syntax-local-introduce`-
   shaped primitive that reads the field. Out of scope for cleanup;
   would need a real design.

3. **Document the intent and leave it as-is.** Add a comment to the
   field explicitly noting it's currently unused and reserved for a
   future `syntax-local-introduce` implementation.

**Recommendation:** (1) for now. The "future primitive" is hypothetical;
when it's actually being built, restoring the field is a five-line
addition. Keeping dead state around for a hypothetical future use is
how (3) tends to rot.

## Cross-reference

Same shape as the `eval_intro_scopes` / `eval_usage_scopes` removals in
the macro-state refactor — both noted as "declared but never read."
The pattern suggests a general invariant worth keeping: every
`EvalConfig`/`EvalState` field should have at least one reader in
production code. A simple grep-based test or a `-Wunused-fields`
audit would catch future regressions.
