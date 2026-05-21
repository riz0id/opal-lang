# `Opal.Parser.parseIdApplication` is non-exhaustive and ignores `letrec`

**Severity:** high (every successfully-expanded program containing
`letrec-syntaxes+values` or any other core form besides `lambda`,
`quote`, `quote-syntax` will crash the parser; the parse phase is
expected to consume the expander's output directly)

**Location:** `packages/opal/src/Opal/Parser.hs:86-91`

## What the code says

```haskell
parseIdApplication :: Identifier -> [Syntax] -> Parse SExp
parseIdApplication id stxs =
  parseIdentifier id >>= \case
    "lambda"       -> parseLambda      [syntax| (lambda ?stxs ...) |]
    "quote"        -> parseQuote       [syntax| (quote ?stxs ...) |]
    "quote-syntax" -> parseQuoteSyntax [syntax| (quote-syntax ?stxs ...) |]
```

Three cases, no fallthrough. The `case` is non-exhaustive — every
other symbol triggers a `Prelude.undefined`-shaped runtime error from
the inexhaustive-patterns warning becoming a runtime exception (or,
under `-Werror`, refuses to compile).

## Why this is on a hot path

`parseSyntax` is the post-expansion parser; it's called via
`runParseSyntax` from `Expander.expanderParse` (`Expander.hs:346-363`).
The expander itself emits forms that aren't in this case list:

* **`letrec`** — produced by `expandLetRec` (`Expander.hs:597`):

  ```haskell
  pure [syntax| (letrec (?vals ...) ?result) |]
  ```

* **General applications** — `expandApplication`
  (`Expander.hs:519-525`) leaves applications headed by non-identifier
  forms alone, but applications headed by identifiers fall through to
  `dispatchCoreForm CoreApp` which preserves the head identifier in
  the output. The parser sees these as `parseIdApplication id [...]`
  with an arbitrary user-defined `id`.

* **`#%app`** — `CoreApp` resolves to symbol `"#%app"`
  (`coreFormString CoreApp = "#%app"`); not in the case list.

* **`begin`** — `CoreBegin` is `"begin"`; not in the case list. (See
  also [[core-begin-and-begin-for-syntax-collide]] for the related
  symbol-collision issue.)

* **`define` / `define-syntax`** — at top-level and module-body these
  remain in the post-expansion tree (the expander preserves them via
  `defineToSyntax` in `partialExpandModuleBegin`).

So basically *every* program reaching the parser hits the
non-exhaustive case. The reason this hasn't surfaced is that the
end-to-end tests evidently don't pipe expanded code through
`runParseSyntax` yet, or only exercise the three pre-existing cases.

## What Racket does

Racket separates *parsing* into a different role: by the time the
expander finishes, the syntax tree is in *fully expanded* form, and the
compiler/parser is a recursive walk over a small, fixed set of fully
expanded forms (Racket's "fully expanded programs" grammar — see
`racket/src/expander/compile/main.rkt`'s `compile` and the grammar in
the documentation). Every core form has a corresponding compile clause;
the dispatch is total. Specifically the grammar is:

```
expr ::= id
       | (#%plain-lambda formals expr ...+)
       | (#%plain-app expr ...+)
       | (quote datum)
       | (quote-syntax datum)
       | (letrec-syntaxes+values ([(id ...) expr] ...)
                                 ([(id ...) expr] ...)
                                 expr ...+)
       | (if expr expr expr)
       | (begin expr ...+)
       | (begin0 expr expr ...)
       | (set! id expr)
       | (with-continuation-mark expr expr expr)
       | (#%expression expr)
       | (#%variable-reference id?)
```

Opal's `Parser` needs cases for at least: `#%app`, `lambda`, `quote`,
`quote-syntax`, `letrec-syntaxes+values` (or whatever Opal's expanded
form for it is — see below), and a general fallthrough for unknown
identifiers (which should error rather than crash silently).

## Secondary issue: `letrec` vs `letrec-syntaxes+values`

`expandLetRec` emits `[syntax| (letrec (?vals ...) ?result) |]`
(`Expander.hs:597`). But:

* No `CoreLetRec` form has symbol `"letrec"` —
  `coreFormString CoreLetRec = "letrec-syntaxes+values"`.
* `letrec` is therefore not bound in the core namespace at all, so
  `parseIdentifier` on it will fail to resolve.

Even if `parseIdApplication` had a `"letrec"` case, the resolution
step would error out first. The expander should emit a form that's
recognizable to the parser. Racket emits `#%letrec-values` (the
post-expansion form, distinct from the source-level
`letrec-syntaxes+values`); Opal should pick a convention and stick to
it.

## Suggested fix

1. Add the missing cases and a fallthrough:

   ```haskell
   parseIdApplication id stxs =
     parseIdentifier id >>= \case
       "lambda"                 -> parseLambda      …
       "quote"                  -> parseQuote       …
       "quote-syntax"           -> parseQuoteSyntax …
       "letrec-values"          -> parseLetRec      …
       "#%app"                  -> parseApp         …
       "begin"                  -> parseBegin       …
       s                        -> fmap SApp (traverse parseSyntax (SyntaxId id :| stxs))
   ```

   The fallthrough handles general user-defined applications (after
   resolving `id` to its binder symbol, treat it as an ordinary
   `SApp`).

2. Settle the `letrec` vs `letrec-syntaxes+values` vs `letrec-values`
   naming. Suggested: rename `CoreLetRec`'s string to
   `"letrec-values"` (Racket's post-expansion form), and emit
   `(letrec-values ...)` from `expandLetRec`. The `expandLetRec`
   guardrail at `Expander.hs:566` should also be updated so the error
   message matches the source-level name (it currently quotes
   `letrec-syntaxes+values`).
