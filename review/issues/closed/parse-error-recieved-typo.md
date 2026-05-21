# `Text.Parse.Error` mis-spells "received" as "recieved" throughout

**Severity:** low (cosmetic; visible in the public API as a field
name and lens, so fixing it has ripple cost)

**Locations:**

* `packages/parse/src/Text/Parse/Error.hs:22` — `parseErrorRecieved`
  lens (re-exported)
* `packages/parse/src/Text/Parse/Error.hs:42` —
  `parse_error_recieved` record field
* `packages/parse/src/Text/Parse/Error.hs:67` — `recieved` parameter
  of `newParseError`
* `packages/parse/src/Text/Parse/Error.hs:84-85` —
  `parseErrorRecieved` lens definition

## What the code says

```haskell
data ParseError = ParseError
  { parse_error_expected :: Set Token
  , parse_error_recieved :: Set Token  -- ← should be `received`
  }

newParseError ::
  Token ->          -- expected
  Token ->          -- recieved      ← should be `received`
  ParseError
newParseError expected recieved = …

parseErrorRecieved :: Lens' ParseError (Set Token)  -- ← should be `parseErrorReceived`
parseErrorRecieved = lens parse_error_recieved …
```

The Haddock comments also use "recieved" (lines 40, 43, 63). Standard
English spelling is **received** (i before e except after c).

## Why it's worth fixing now (and not later)

The misspelling is in the public API — the field accessor, the lens,
the constructor argument name. The longer it stays, the more
downstream code references the misspelled identifier and the more
expensive a rename becomes.

Currently the package is orphaned from `opal` (no callers), so the
rename is purely intra-package and has zero downstream impact today.
This is the easiest moment to fix it.

## Suggested fix

Find/replace `recieved` → `received` and `Recieved` → `Received`
across `Text/Parse/Error.hs`. Five identifier renames plus a handful
of comment fixes. ~10-line diff.

Verify with a build:

```
nix-shell --run 'cabal new-build all'
```

No other modules currently import `Text.Parse.Error` (per
`grep -r 'Text.Parse.Error' packages/`), so no consumer updates
needed.

## Aside

This typo is *also* present in `Opal.Common.Unicode` and possibly
elsewhere in the broader project. A separate pass to grep for
`recieved` across the whole tree and fix would tidy up the lot.
