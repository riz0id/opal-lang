# `Text.Parse.Error.ParseError`'s `Ord` instance has no meaningful semantics

**Severity:** low (cosmetic / latent footgun — current code doesn't
use it as an ordered key, but the derived `Ord` will silently work
even when ordering parse errors makes no sense)

**Location:** `packages/parse/src/Text/Parse/Error.hs:38-45`

## What the code says

```haskell
data ParseError = ParseError
  { parse_error_expected :: Set Token
  , parse_error_recieved :: Set Token
  }
  deriving (Eq, Ord, Show)
```

`Ord` is derived. The order is lexicographic over the two `Set Token`
fields. Two parse errors are compared first by their *expected* token
set, then by their *received* token set, both via `Set`'s `Ord`
(which compares as a sorted sequence).

This ordering has **no semantic meaning** for parse errors. The order
of errors is not "more relevant first" or "earlier in input first" or
any other sensible criterion — it's just the alphabetical ordering of
their Token sets. There's no real-world consumer that benefits from
it.

## Why it matters (a little)

Two risks:

1. **Silent map-key usage.** A future contributor sees `Ord
   ParseError` is in scope and reaches for `Map ParseError v` or `Set
   ParseError` to deduplicate errors. The map will *function*, but
   the dedup is by tokens, not by error identity (e.g. position), so
   identical-tokens-different-position errors collapse together.

2. **Sorting for display.** Code that sorts errors before
   printing — `sort errors :: [ParseError]` — will get an order that
   bears no relationship to source position or severity. Misleading.

Compare to Megaparsec's `ParseError`, which carries position info and
intentionally does *not* derive `Ord` — sorting errors there requires
explicitly choosing the criterion.

## Suggested fix

Drop the `Ord` deriving:

```haskell
data ParseError = ParseError
  { parse_error_expected :: Set Token
  , parse_error_recieved :: Set Token
  }
  deriving (Eq, Show)
```

If future code legitimately needs to order errors (e.g. "earliest
position first" once positions are added — see
[[parse-combinator-library-incomplete]]), add a deliberate `Ord`
instance keyed on whatever the actual ordering criterion is.

## Aside

The "recieved" misspelling in `parse_error_recieved` is its own
issue — see
[[parse-error-recieved-typo]].
