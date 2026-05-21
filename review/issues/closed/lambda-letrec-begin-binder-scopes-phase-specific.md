# Lambda, letrec, and begin binder scopes are attached phase-specifically

**Severity:** high (every binder introduced by `lambda`, `letrec`, or
`begin` lives in the wrong half of `ScopeInfo`; reference resolution at
a non-base phase will not see the binder; manifests as soon as a
macro expansion produces a binder at one phase and uses it at another)

**Locations:**

* `packages/opal/src/Opal/Expander.hs:561-570` — `expandLambda`
* `packages/opal/src/Opal/Expander.hs:593-620` — `expandLetRec`
* `packages/opal/src/Opal/Expander.hs:649-662` — `expandBegin`
  (outside-edge and inside-edge applications)
* `packages/opal/src/Opal/Expander.hs:303-314` — `scopeId True` /
  `scopeSyntax True` (the `Bool = True` branch that adds at the
  current phase only)

## What the code says

The same `True` / `Just ph` mistake from
[[intro-and-use-site-scopes-are-phase-specific]] is repeated here, but
for the *binder-introducing* scopes that lambda, letrec, and begin
create. Every place the expander allocates a fresh scope for an
internal binder, it then attaches that scope phase-specifically:

```haskell
-- expandLambda
sc <- newScope
bindings <- for ids \id -> do
  id'  <- scopeId True sc id           -- phase-specific!
  bind <- newBinding id'
  pure (bind, id')

withVarTransformers bindings do
  let args = map snd bindings
  stx'   <- scopeSyntax True sc expr   -- phase-specific!
  …
```

```haskell
-- expandLetRec
valBinds <- for valExprs \(valId, valExpr) -> do
  valId'   <- scopeId True sc valId             -- phase-specific
  valExpr' <- scopeSyntax True sc valExpr       -- phase-specific
  …

transBinds <- for transExprs \(transId, transExpr) -> do
  transId'   <- scopeId True sc transId         -- phase-specific
  transExpr' <- scopeSyntax True sc transExpr   -- phase-specific
  …

…
scoped <- scopeSyntax True sc expr              -- phase-specific
```

```haskell
-- expandBegin
outsideEdgeScope <- newScope
outsideEdgeStxs  <- traverse (scopeSyntax True outsideEdgeScope) stxs   -- phase-specific
…
insideEdgeScope  <- newScope
…
scopeSyntax True insideEdgeScope result          -- phase-specific
```

`scopeId True` and `scopeSyntax True` (`Expander.hs:303-314`) both
look up the current expansion phase and call `identifierScope (Just
ph)` / `syntaxScope (Just ph)` — i.e. they route through the per-phase
`MultiScope` slot, not through `gscps`.

## What Racket does

Racket's scope-sets model has exactly two kinds of scope:

1. **Plain scopes** — including the scopes introduced by `lambda`,
   `letrec-values`, `let-values`, internal-definition bodies, intro
   scopes, use-site scopes. These live in a syntax object's *global*
   scope set; they're visible at every phase. The whole point of a
   "plain scope" in the scope-sets paper §3 is that it's not
   phase-stratified.

2. **Multi-scopes / representative-scopes** — the per-phase members of
   the multi-scope attached to a module. These are the only thing
   that should ever land in the per-phase store.

Racket's `add-scope` (`racket/src/expander/syntax/scope.rkt`) routes
through `generalize-scope`, which returns the scope unchanged unless
it's specifically a `representative-scope`. Plain scopes go into the
*set* portion of the scope info, not the multi-scope portion.

The scope-sets paper makes this explicit in §3 ("Resolving Bindings"):
"a reference resolves to the binding whose scope set is the largest
subset of the reference's scope set" — and the scope set is the union
of plain scopes plus the per-phase representative scopes. A fresh
lambda binder scope is a plain scope; it must show up in that union
at every phase, otherwise references can't see the binding from the
phase they're resolved at.

## Concrete failure mode

```scheme
(define-syntax m
  (lambda (stx)
    ;; ... transformer body that constructs (lambda (x) x) at phase 1
    ))

(m)
```

When `(m)` is expanded at phase 0:

1. `applyTransformer` runs the transformer at phase 1 (`nextPhase`),
   which constructs the syntax `(lambda (x) x)`.
2. The constructed syntax flows back to the caller and gets
   `expand`'d at phase 0.
3. At phase 0, `expandLambda` is called on `(lambda (x) x)`.
4. `scopeId True sc x` adds `sc` to the per-phase-0 slot of the bound
   `x`.
5. `scopeSyntax True sc expr` adds `sc` to the per-phase-0 slot of
   the body's `x` reference.
6. The body's `x` resolves to the binder `x` because both have `sc`
   in their phase-0 slot.

So at phase 0 it appears to work. But:

* If the body is later visited at phase 1 (e.g. via `quote-syntax`
  passed back up through another macro), the phase-1 lookup of `x`
  does NOT see `sc` (it's in the phase-0 slot), and resolution fails
  with `not-in-scope`.

* Worse, in `expandLetRec`'s `transBinds` branch
  (`Expander.hs:599-607`), the transformer is added at the *current*
  phase (let's call it `ph`) before `nextPhase`:

  ```haskell
  transBinds <- for transExprs \(transId, transExpr) -> do
    transId'   <- scopeId True sc transId          -- at phase ph
    transExpr' <- scopeSyntax True sc transExpr    -- at phase ph
    binder     <- newBinding transId'

    nextPhase do
      sexp  <- expandAndParseSyntax transExpr'    -- now at phase ph+1
      value <- expanderEval Nothing sexp
      pure (binder, value)
  ```

  The transformer expression `transExpr` is scoped at `ph`, then
  expanded/parsed at `ph+1`. The references inside `transExpr` that
  should resolve to `letrec-syntaxes`-bound identifiers won't see
  `sc` (it's in `ph`'s slot, not `ph+1`'s) — so the transformer body
  cannot reference its own letrec-syntaxes neighbours, contradicting
  the form's defining feature.

## Why it isn't yet visible at base phase 0

Same reason as [[intro-and-use-site-scopes-are-phase-specific]]: the
existing test surface mostly runs at phase 0, so the broken
phase-specific attachment happens to land in the same slot the
resolver later reads. The first multi-phase test surfaces it. Note
that **the macro-state refactor closed three related issues but
explicitly left this one out of scope** (see
`plans/expander-macro-state-refactor.md` "Out of scope"). This is the
follow-up.

## Suggested fix

Two layers:

### Tactical: change every `True` to `False`

The `scopeId`/`scopeSyntax` `Bool` API already supports phase-
independent attachment via `False`. Mechanical sed:

* `Expander.hs:562, 568` (`expandLambda`)
* `Expander.hs:594, 595, 600, 601, 617` (`expandLetRec`)
* `Expander.hs:654, 662` (`expandBegin`)

All change from `True` to `False`. Nine call sites.

### Structural: remove the `Bool` API entirely

The `Bool` parameter is a long-standing code smell flagged in
[[intro-and-use-site-scopes-are-phase-specific]]:

> Note that `scopeId`'s `Bool` parameter (`True = phase-specific,
> False = phase-independent`) is also a code smell — switch the API
> to `Maybe Phase` everywhere and let callers be explicit.

After the tactical fix all call sites pass `False`, so `scopeId` and
`scopeSyntax` become trivial wrappers around `identifierScope
Nothing` / `syntaxScope Nothing`. Delete them and call those
directly. ~5-line reduction; better signal at every call site.

The only place that *should* keep phase-specific attachment is the
module inside-edge scope, which is already handled by
`addInsideEdgeScope` in the `DefinitionContext` machinery (uses
`syntaxScope Nothing` post-refactor — note that even inside-edge is
phase-independent in the current implementation; the per-phase nature
of module multi-scopes is captured by the *scope's identity*, not by
which slot it lives in on a syntax object).

## Aside: `expandBegin` allocates a redundant inside-edge scope

`expandBegin` allocates `insideEdgeScope <- newScope` and applies it
to `result`. But `expandBegin` *also* calls `preExpandBegin`, which
calls `withDefinitionContext` (post-refactor: allocates its own
`DefinitionContext` with an inside-edge scope). The two inside-edge
scopes are not the same — one is a fresh scope created in
`expandBegin`, the other is the per-`DefinitionContext` scope used by
`addInsideEdgeScope`. Macros expanded *inside* `preExpandBegin`'s
`withDefinitionContext` block see the latter; the `result` of
`expandBegin` gets the former applied on top. They overlap
conceptually.

Racket has *one* inside-edge scope per definition context. After this
fix, the `expandBegin` line 662 application should also be deleted
(or moved inside the `withDefinitionContext`), and the inside-edge
scope should come from the active `DefinitionContext` via
`addInsideEdgeScope`. Worth its own follow-up issue.
