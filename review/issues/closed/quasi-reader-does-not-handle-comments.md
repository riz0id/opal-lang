# `Opal.Quasi.Reader` does not handle comments

**Severity:** medium (comments inside `[syntax| ... |]` quasiquoter
patterns cause Template Haskell compile errors. The main reader was
fixed to handle line/block comments; the quasi reader was not updated
in parallel and uses Megaparsec's bare `space` skipper.)

**Location:** `packages/opal/src/Opal/Quasi/Reader.hs:45,68,74,95`

## What the code says

```haskell
import Text.Megaparsec.Char (space, string)
…

readQExp :: Reader QExp
readQExp = do
  space                              -- bare space skipper
  qexp <- choice
    [ try readQuasiBool
    , readQuasiVar
    , readQuasiList
    ]
  qexp <$ space                      -- bare space skipper

…

readQuasiVar :: Reader QExp
readQuasiVar = do
  s <- readSymbol <* space           -- bare space skipper
  …
```

All three `space` calls are `Text.Megaparsec.Char.space`, which only
consumes Unicode whitespace — not comments. This is the same
comment-blindness bug that `Opal.Reader` had before the recent fix
(closed
`review/issues/closed/` — added the
`skipSpace` helper composed via `Text.Megaparsec.Char.Lexer.space`).

`Opal.Quasi.Reader` was flagged as "out of scope" in the original
comment-support plan:

> Comments inside `[syntax| … |]` quasiquoter — separate code path
> (`Opal.Quasi.Reader`) that may or may not share with `Opal.Reader`.

It is a separate code path: `Opal.Quasi.Reader` does *not* import
`Opal.Reader.skipSpace`, so the main reader's fix doesn't reach the
quasiquoter.

## Concrete failure mode

Any Haskell source file that writes a comment inside a `[syntax| … |]`
quasiquote:

```haskell
[syntax|
  (lambda (x)
    ;; double the argument
    (* x 2))
|]
```

…fails at compile time with a parse error from `runQuasiReader` (the
Megaparsec parser triggered by `Opal.Syntax.TH.syntax`). The error
points at the `;`, complaining about an invalid character.

This affects:

* Source files inside the Opal library itself that use `[syntax| ... |]`
  to construct/match syntax — currently mostly comment-free, but the
  hygiene of adding a comment is non-obvious.
* Future user-facing macro-definition forms that embed quasiquoted
  syntax with documentary comments.
* The test suite's quasi-quoter generators.

## Suggested fix

Mirror the main reader's fix: introduce a `skipSpace` helper in
`Opal.Quasi.Reader` (or expose and reuse `Opal.Reader.skipSpace`),
then replace the three `space` calls.

Two ways to spell this:

### Option A — reuse `Opal.Reader.skipSpace`

```haskell
import Opal.Reader (Reader (..), ReaderError (..), readEnclosed, readSymbol, skipSpace)
```

Then `s/space/skipSpace/g`. Requires `skipSpace` to be in
`Opal.Reader`'s export list (it is exported from the module but worth
verifying — it's referenced as a top-level function in
`Opal.Reader.hs`).

Pros: single source of truth; comment syntax stays in sync between the
main reader and the quasi reader by construction.

Cons: introduces an `Opal.Reader` → `Opal.Quasi.Reader` link (already
exists for `Reader`, `ReaderError`, `readEnclosed`, `readSymbol` — so
no new dependency).

### Option B — duplicate the helper locally

```haskell
skipSpace :: Reader ()
skipSpace = L.space space1 (skipLineComment ";") (skipBlockCommentNested "#|" "|#")
```

Pros: no cross-module change.

Cons: two definitions of "what counts as whitespace in Opal source"
that can drift apart.

**Recommendation: Option A.** The cross-module link is one-way
(`Quasi.Reader` already imports `Reader`), and a single source of
truth prevents the comment syntax from drifting.

## Regression test

A test in `Test.Opal.Quasi` (file exists per the cabal `other-modules`
list) — or a smaller test in `Test.Regression` — that exercises
`[syntax| ... |]` with embedded comments:

```haskell
[syntax|
  ;; this should be skipped
  #t
|] === [syntax| #t |]
```

Plus the inside-list and trailing variants from the main reader's
comment tests.

## Cross-reference

Implementing Option A would close the "may or may not share with
`Opal.Reader`" question raised in the closed comment-support plan in
favour of "shares."
