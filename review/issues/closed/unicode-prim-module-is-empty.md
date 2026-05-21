# `Data.Unicode.Prim` is declared but exports nothing

**Severity:** low (cosmetic / cabal hygiene; an empty module in
`exposed-modules` is dead weight but not a bug)

**Locations:**

* `packages/unicode/src/Data/Unicode/Prim.hs` (the empty module)
* `packages/unicode/unicode.cabal` (lists `Data.Unicode.Prim` in
  `exposed-modules`)

## What the code says

```haskell
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Data.Unicode.Prim
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Data.Unicode.Prim
  (
  )
where
```

The module declares an explicit empty export list and contains no
definitions. It compiles (because nothing is exported and nothing
references it), but adds no value.

`Data.Unicode` (the sibling module) already contains all the
primitive UTF-8 functions — `ord1#`/`ord2#`/etc. and the `#`-suffixed
unsafe primitives. There's no obvious thing left for
`Data.Unicode.Prim` to hold.

## Suggested action

Two options:

### Option A — delete it

If there's no plan to populate the module, remove it. One line
removed from `exposed-modules` in `unicode.cabal`, one file deleted
from `src/Data/Unicode/`. Reduces surface area.

### Option B — populate it

If the intent was to move the `#`-suffixed unsafe primitives
(`ord1#`, `chr4#`, etc.) from `Data.Unicode` into `.Prim` as a
separation between "safe boxed" and "unsafe unboxed" APIs, do that
move. Then `.Prim` becomes the internal layer and `Data.Unicode`
re-exports only the safe boxed versions.

The same separation appears in `Data.Text.Internal` /
`Data.Text.Internal.Unsafe` etc. — would be a clean refactor and
gives a place for future `chr4#` to live without polluting the main
namespace.

**Recommendation: B**, but contingent on the
[[unicode-decoding-does-not-validate-utf8]] fix landing first. Once
the validating boxed versions exist, moving the unchecked primops
to `.Prim` is a natural step.

If neither (A) nor (B) gets prioritized, the module is harmless —
this is the lowest-severity issue in the `unicode` package review.

## Aside

If the package gets the deduplication treatment from
[[unicode-duplication-with-opal-common-unicode]], the empty `.Prim`
module may end up being the natural home for the `#`-suffixed
primops migrated over from `Opal.Common.Unicode`. Worth deciding the
layout when that work lands.
