# Plan — template-construction primitives

Build out the macro-author surface from "essentially nothing"
(`quote-syntax` only) to enough that `lib/define.opal`'s aspirational
`plain-define` and `for` macros — and the canonical
`define-syntax-rule` idiom — can be expressed.

This is a multi-stage effort. Each stage is its own commit, gated by
review.

## Current state

The transformer language is the lambda calculus over `Datum`s. The
evaluator (`Opal/Evaluator.hs:evalSExp`) handles only:

```haskell
SVal val      -> pure val
SVar var      -> getVariable var
SApp (f :| _) -> case f of DatumLam fun -> evalSApp fun …
                           _            -> error "not a function"
```

There are no primitives. Every binding has to be a `DatumLam` or
`DatumVal`/`DatumStx`/`DatumList` literal. A transformer can therefore
only:

1. Return `(quote-syntax …)` literal.
2. Apply user lambdas to user values.

Adding *any* useful template primitive requires extending the
evaluator with a way to call into Haskell. Once that exists, the
remaining stages are mostly "pile on more primitives" or "add a core
form that compiles to primitive calls".

## Architecture decision

**Add a new `Datum` constructor for primitives:**

```haskell
-- packages/opal/src/Opal/Syntax.hs
data Datum
  = …
  | DatumPrim Primitive

-- packages/opal/src/Opal/Syntax/Primitive.hs (NEW)
data Primitive = Primitive
  { prim_name  :: Symbol
  , prim_arity :: Int
  , prim_apply :: [Datum] -> Eval Datum
  }
```

Pros:
- Distinct from `DatumLam`, so we never confuse "user-defined lambda
  body, evaluable as SExp" with "Haskell function, evaluable via
  prim_apply."
- Keeps the existing `evalSApp` (`Lambda -> [SExp] -> Eval Datum`)
  unchanged; we add a parallel `evalPrimApp :: Primitive -> [SExp]
  -> Eval Datum` that evaluates arguments and forwards to
  `prim_apply`.

Cons:
- Requires extending the existing `{-# COMPLETE … #-}` pragma on
  `Datum` (`Syntax.hs:177-179`) to include `DatumPrim`. Mechanical.

Alternative considered: extend `Lambda` to carry either an `SExp`
body or `[Datum] -> Eval Datum`. Rejected — every site that pattern
matches on `Lambda` would need updating, and the type would be
semantically heterogeneous.

## Stage 1 — primitive infrastructure + kernel inspectors

The floor. Adds the ability for transformers to inspect syntax.

### Adds

* `Opal.Syntax.Primitive` module (new):
  - `Primitive` record.
  - `evalPrimApp :: Primitive -> [SExp] -> Eval Datum`.
* `Datum` gains `DatumPrim Primitive` constructor; update the
  `COMPLETE` pragma and `Display`/`NFData` instances.
* `Opal.Evaluator.evalSExp`'s `SApp` branch: pattern matches on
  `DatumLam` *and* `DatumPrim`.
* `Opal.Primitives` module (new) — bundles the primitive table and
  installs them into the initial `Environment` + `BindingStore`:
  - **List primitives:** `car`, `cdr`, `cons`, `null?`, `pair?`,
    `list`.
  - **Equality:** `eq?` (symbol identity), `equal?` (structural).
  - **Syntax inspectors:**
    - `syntax-e :: Syntax -> Datum` — peel one layer.
    - `syntax->list :: Syntax -> Maybe [Syntax]` — list-shaped only.
    - `syntax->datum :: Syntax -> Datum` — recursive strip.
    - `datum->syntax :: Syntax -> Datum -> Syntax` — lift datum with
      context syntax's lexical info.
    - `identifier? :: Datum -> Bool`.
    - `syntax? :: Datum -> Bool`.
* `coreEnvironment` and `coreBindingStore` get extended to include
  the primitives, so they're imported alongside the existing core
  forms via `(import #%core)`.

### Removes

Nothing.

### Files touched

- `packages/opal/src/Opal/Syntax.hs` — `DatumPrim` constructor,
  COMPLETE pragma update.
- `packages/opal/src/Opal/Syntax/Primitive.hs` — **NEW**.
- `packages/opal/src/Opal/Primitives.hs` — **NEW**, ~150 lines.
- `packages/opal/src/Opal/Evaluator.hs` — `SApp` dispatch on
  `DatumPrim`.
- `packages/opal/src/Opal/Binding/Environment.hs` — `coreEnvironment`
  registers primitives.
- `packages/opal/src/Opal/Binding/BindingStore.hs` — `coreBindingStore`
  registers primitives' symbols.
- `packages/opal/opal.cabal` — add the two new modules to
  `exposed-modules`.

### Regression test

A new `Test.Regression.primitivesAreReachableFromMacros` group:
- A `(define-syntax foo (lambda (stx) (car (cdr (syntax->list stx)))))`
  — verify the macro can call primitives without error.
- `syntax-e` peels exactly one layer.
- `datum->syntax` round-trips with `syntax->datum`.

~30 lines of test.

### Effort

~250 lines of Haskell + tests. One commit.

### Verification

`lib/define.opal`'s `plain-define` can now be written in verbose
form (modulo also needing `let`, see Stage 1.5):

```scheme
(define-syntax plain-define
  (lambda (stx)
    ;; stx is (plain-define id rhs); cdr of syntax-e gives (id rhs).
    (datum->syntax stx
      (cons (quote-syntax define)
            (cdr (syntax-e stx))))))
```

If `let` is available (see 1.5), the inner can use it for readability;
otherwise the above is acceptable.

## Stage 1.5 — `let` as a derived form

`let` is a Racket macro that lowers to `letrec-values`/`lambda`.
Without it, transformer bodies become unbearably nested.

Two options:

**A. Add `let` as a CoreForm.** Simpler — one new entry in
`CoreForm`, one new `dispatchCoreForm` clause that lowers to
`letrec-syntaxes+values`. ~30 lines.

**B. Implement `let` as a built-in transformer.** Cleaner long-term
— `let` ships as a transformer in `#%core`'s environment, defined
in terms of Stage 1's primitives + Stage 3's `syntax-case`. Requires
Stage 3, so defer.

**Recommendation: A** for now. Cost is low; refactoring to B later is
straightforward.

### Effort

~50 lines. Optional but strongly recommended — without it, anything
beyond trivial transformers is painful.

## Stage 2 — `syntax` template form (without patterns)

The literal-template form without surrounding pattern variables. Use:

```scheme
(syntax (let ([t #t]) t))
```

This is equivalent to `(quote-syntax …)` at this point (no pattern
variable substitution because no patterns exist yet). Worth adding as
a separate CoreForm so the parser / quasi-quoter syntax stabilises
before Stage 3 layers patterns on top.

**Defer** until Stage 3 — `syntax` is only interesting when patterns
exist. Don't add it standalone.

## Stage 3 — `syntax-case` and pattern templates

The big lift. Adds:

- `(syntax-case stx (literals …)
     [pattern template]
     …)` core form.

Patterns:
- `_` matches anything, binds nothing.
- `id` matches anything, binds `id` as a pattern variable.
- `(p1 p2 … pn)` matches a list of exactly that shape.
- `(p1 … . prest)` matches a list with rest.
- `(p ...)` matches zero or more — ellipsis. Each pattern variable
  inside binds a *list* of matches (depth = number of enclosing
  `...`).
- Literal identifiers in the literals list match by `free-identifier=?`.

Templates:
- `id` — if `id` is a pattern variable, substitutes; otherwise
  emitted as a literal identifier.
- `(t1 … tn)` — emits a list-shaped syntax with elements from
  templates.
- `(t ...)` — splices a depth-1 pattern variable's matches.

Implementation breakdown:

### 3a. Pattern compiler

Walks the pattern AST, produces:
- A list of pattern variables with their ellipsis depth.
- A matcher function `Syntax -> Maybe Bindings`, where `Bindings` is
  a map from pattern variable to either a single `Syntax` (depth 0) or
  a `[Syntax]` (depth 1) or `[[Syntax]]` (depth 2), etc.

### 3b. Template walker

Walks the template AST, given the bindings, produces a `Syntax`.
Ellipsis in templates iterates over the corresponding depth-1+ pattern
variable's matches.

### 3c. CoreForm + dispatch

- New `CoreSyntaxCase`.
- `dispatchCoreForm CoreSyntaxCase` in the expander processes the
  pattern/template list and produces an `SExp` that, when evaluated,
  performs the matching and template construction at *macro-expansion
  time*.

Crucially: the matching happens *at the time the macro is invoked*,
not at the time the macro is defined. So `syntax-case` compiles into
an SExp that calls the primitives from Stage 1 — `syntax-e`,
`syntax->list`, `car`/`cdr`, plus the new pattern-matcher and
template-walker primitives.

### Files touched

- `Opal.Core` — add `CoreSyntaxCase`.
- `Opal.Expander` — `dispatchCoreForm CoreSyntaxCase` (the lowering).
- `Opal.Syntax.Pattern` — **NEW**, the pattern AST + compiler.
- `Opal.Syntax.Template` — **NEW**, the template AST + walker.
- `Opal.Primitives` — extend with `pattern-match`, `template-fill`
  primitives if the lowering goes that route. Or implement the
  whole thing in Haskell and expose a single `syntax-case-helper`
  primitive.

### Effort

~600 lines of Haskell + tests. The pattern matcher is the most
involved piece — needs careful design around ellipsis depth.

This is the stage that "makes Racket macros work." Worth a dedicated
plan document of its own before starting.

## Stage 4 — library macros

Once Stages 1-3 land, the following ship as transformers in the
prelude (not compiler changes):

- `syntax-rules` — sugar over `syntax-case` with no fender clauses.
- `define-syntax-rule` — sugar over `define-syntax` + `syntax-rules`.
- `with-syntax` — binding form using `syntax-case`.
- `quasisyntax` / `#`` — template with escape, expressible via
  `with-syntax`.

All ~50 lines each. Order: `syntax-rules` first (unblocks
`define-syntax-rule`), then `with-syntax`, then `quasisyntax`.

## Out of scope

- `syntax-parse` (Racket's typed syntax-class pattern matcher) — a
  much larger effort; can come after Stage 4 if needed.
- Module phase machinery for transformers that *use* transformers
  (cross-phase macro libraries). The current `nextPhase` handling
  works for one phase up; multi-phase macros are a separate problem.
- Custom reader extensions, `read-syntax`, etc.

## Suggested commit sequence

| Commit | Stages | What works after |
|---|---|---|
| 1 | 1 | Transformers can inspect syntax via primitives. Macros writeable in verbose `cons`/`datum->syntax` style. |
| 2 | 1.5 | `let` available in transformer bodies. |
| 3 | 3a | Pattern compiler standalone (with unit tests, no expander integration yet). |
| 4 | 3b | Template walker standalone (unit tests). |
| 5 | 3c | `syntax-case` integrated into the expander. End of "real macros work." |
| 6 | 4 | Library macros — `syntax-rules`, `define-syntax-rule`, `with-syntax`, `quasisyntax`. |

Stages 3a-3c could collapse into one big commit if the design is
clear enough; splitting helps review.

## Design decisions (resolved)

The plan originally flagged four open questions. After working
through them, the answers are:

### Q1 — Pattern-variable depth representation

**Decision: nested `Match` ADT + separate `PatternDepth` map.**

```haskell
-- Opal.Syntax.Pattern (new)
data Match
  = MOne  Syntax       -- depth-0 binding
  | MMany [Match]      -- depth-(n+1): list of depth-n matches

newtype Bindings = Bindings (Map Symbol Match)

type PatternDepth = Map Symbol Int   -- depth per pattern variable,
                                     -- produced by the pattern
                                     -- compiler, consumed by the
                                     -- template type-checker
```

Mirrors Racket's internal nested-list representation. Template
type-checking (template's `...` count must match pattern variable
depth) happens at *template-compile* time via `PatternDepth`, not at
runtime. Considered GADT-typed `DepthMatch (n :: Nat)` — rejected
because depth-2+ requires existential wrappers without buying
anything beyond compile-time check we can do with `PatternDepth`.

### Q2 — Matcher/walker location

**Decision: Haskell, integrated via a new `SExp` constructor — not
via primitive lookup.**

```haskell
data SExp
  = SVal  Datum
  | SVar  Symbol
  | SApp  (NonEmpty SExp)
  | SCase SExp [(Pattern, Template)]   -- NEW
```

Rationale: we have no existing macros to bootstrap `syntax-case` as
a macro that lowers to primitive calls (Racket can; we can't). So
"lower to primitive calls" would mean encoding `Pattern`/`Template`
ASTs as `Datum` lists and decoding at primitive-call time. The
direct `SCase` constructor avoids that encoding round-trip, keeps
the matcher as ordinary Haskell pattern matching, and gives one
evaluator dispatch per `syntax-case` invocation. Cost: three
mechanical updates (evaluator, parser, expander) — one-time.

### Q3 — Hygiene of template-introduced identifiers

**Decision: no special wiring at template-walker time. The existing
`applyTransformer` intro-flip handles it.**

Mechanism (scope-sets paper §4):

1. `withIntroScope` allocates fresh intro scope `s`.
2. Input `flipSyntax` adds `s` to input identifiers.
3. Template literal identifiers carry the *macro definition's* scope
   set (preserved by the walker via the literal template syntax
   objects' existing `SyntaxInfo`).
4. Output `flipSyntax` removes `s` from input-derived identifiers
   (they had it from step 2) and adds `s` to template-introduced
   identifiers (they didn't).
5. Resolution: template-introduced identifiers have
   `{macro_def_scopes ∪ {s}}` — bindings from the macro's
   definition site (subset of `macro_def_scopes`) match; use-site
   bindings don't (they'd need scopes the template doesn't have).

**Implication for the walker**: store literal template identifiers
as their original `Syntax` objects. Substitute only pattern
variables. The `applyTransformer` flip does the rest.

**Caveat for list-shape construction**: the outer wrapper of a
constructed `(t1 … tn)` needs a `SyntaxInfo` — use the template's
own source-syntax info (macro-definition context). Same intuition.

### Q4 — Source-location handling

**Decision: default to template-form source locations. Defer
`syntax/loc` to Stage 4 as a library macro.**

For Stages 1-3:

- `datum->syntax ctxt v` carries `ctxt`'s source location naturally
  via `SyntaxInfo.stx_info_source`.
- Template-constructed syntax inherits source location from the
  template form's literal syntax — no walker work needed.

For Stage 4, `syntax/loc` ships as `syntax-rules` sugar:

```scheme
(define-syntax syntax/loc
  (syntax-rules ()
    [(_ src tmpl) (datum->syntax src (syntax->datum (syntax tmpl)))]))
```

No compiler work at Stages 1-3.

## How to proceed

Stage 1 is self-contained and immediately useful — I'd suggest
starting there, executing it, then re-evaluating scope before
committing to Stage 3. Stages 2 and 4 are small and can be appended
as needed.

The big-rock decision is Stage 3. It's a real design exercise that
benefits from its own dedicated plan document once Stage 1 is
landed.
