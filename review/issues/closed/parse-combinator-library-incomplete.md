# `Text.Parse` exposes only 5 combinators — missing the standard library

**Severity:** medium (the library has bones but not flesh; nothing
nontrivial can be written against the current public API)

**Location:** `packages/parse/src/Text/Parse.hs:14-27` (the exported
list)

## What the code says

```haskell
module Text.Parse
  ( -- * Parse
    Parse (..),
    -- ** Basic Operations
    parse,
    runParse,
    -- * Combinators
    consume,
    single,
    string,
    try,
  )
where
```

Five public combinators:

* `consume :: Parse (Maybe Char)` — consume one char if available
* `single :: Char -> Parse ()` — match a specific char
* `string :: String -> Parse ()` — match a literal string
* `try :: Parse a -> Parse a` — backtracking try

Plus `parse`/`runParse` (top-level runners).

`Parse` derives `Alternative` (via `ExceptT`+`StateT` over `IO`), so
`<|>` exists. But the *combinators that depend on `Alternative` and
return useful results* aren't exposed.

## What's missing

For a parser combinator library to be usable, the following are
table stakes:

| Combinator | Purpose |
|---|---|
| `many`, `some` | zero/one-or-more repetition with result collection |
| `choice` | try a list of alternatives |
| `alt` / `(<|>)` exposed | shadow `<\|>` or re-export from `Alternative` |
| `sepBy`, `sepBy1`, `endBy`, `sepEndBy` | comma-separated lists, etc. |
| `between` | bracketed forms — `between (single '(') (single ')') p` |
| `optional` | zero-or-one |
| `lookAhead`, `notFollowedBy` | non-consuming inspection |
| `eof` | match end of input (currently impossible — see [[parse-consume1-silently-returns-nul-at-eof]]) |
| `satisfy :: (Char -> Bool) -> Parse Char` | conditional consume |
| `char`-class predicates: `digit`, `letter`, `space`, etc. | character class matchers |
| `manyTill p end` | run `p` until `end` succeeds |
| Position tracking: `getPosition`, `getOffset` | error context |

Without these, the only parsers expressible are literal-string
matches in fixed order — useful for `"begin"` keyword detection but
not for, say, parsing a lambda form or matching a number.

## Why this matters

The package is positioned (per the directory layout) as a
self-contained parser library and possibly as a replacement /
complement to the Megaparsec-based `Opal.Reader`. Megaparsec ships
all of the above out of the box; replacing it without these would be
a regression for any consumer.

Currently the package is orphaned — `packages/opal/` doesn't depend
on `parse` — so this isn't blocking anyone today. But the library
isn't suitable for migration to until at least:

1. `many`, `some`, `optional`, `choice`, `sepBy`, `between`,
   `satisfy`, and `manyTill` are exposed.
2. `eof` and position tracking work (need EOF tokens — see
   [[parse-consume1-silently-returns-nul-at-eof]] — and a state
   shape that includes a line/column position).

## Suggested fix

Add the combinator zoo to `Text.Parse`. Most are 1–5 lines each on
top of `Parse`'s `Alternative` instance:

```haskell
many :: Parse a -> Parse [a]
many p = (:) <$> p <*> many p <|> pure []

some :: Parse a -> Parse [a]
some p = (:) <$> p <*> many p

choice :: [Parse a] -> Parse a
choice = foldr (<|>) (throwError mempty)

satisfy :: (Char -> Bool) -> Parse Char
satisfy pred = do
  c <- consume1
  when (not (pred c)) (throwError …)
  pure c

between :: Parse open -> Parse close -> Parse a -> Parse a
between open close p = open *> p <* close
```

…and so on. A weekend's worth of code for the bulk of it.

For position tracking, `ParseState` (currently just `Buffer + Int`)
needs a line + column field that `consumeParseState` maintains.

## Out of scope

A streaming variant of the buffer (see
[[parse-no-streaming-full-input-required]]) is a deeper restructure
and doesn't need to land at the same time.
