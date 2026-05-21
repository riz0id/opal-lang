# `Data.Unicode` exports 20+ functions all marked `-- TODO: docs`

**Severity:** low (no consumer affected because the package is
orphaned; but the moment the `unicode` package gets a real user, the
doc gap blocks reading the API)

**Locations:**

* `packages/unicode/src/Data/Unicode.hs` — every exported function
  has `-- | TODO: docs` immediately above. ~21 instances.
* `packages/unicode/src/Data/Unicode/TH.hs` — same.
* `packages/unicode/unicode.cabal` — `synopsis: TODO` and
  `description: TODO`.

## What's missing

The package exposes a tight, performance-oriented API:

* Encoding: `ord1`, `ord2`, `ord3`, `ord4` (and their `#` primops)
* Decoding: `chr1`, `chr2`, `chr3`, `chr4` (and `#` primops)
* Pointer ops: `readUtf8OffPtr`, `writeUtf8OffPtr`, `readWord8OffPtr`,
  `writeWord8OffPtr`
* Size queries: `sizeofLeaderUtf8`, `sizeofCharUtf8`
* TH: `staticListE`, `listToBytes`

For each, a reader of the API needs to know:

* **What it takes** — `ord3` takes a `Char` that fits in three UTF-8
  bytes (`U+0800..U+FFFF`). Out-of-range input produces nonsense.
  Currently undocumented.
* **What it returns** — `ord3 :: Char -> (Word8, Word8, Word8)`.
  The tuple order is leader, continuation 1, continuation 2 — not
  obvious from the type.
* **Preconditions / partial-ness** — most of the `chr` /`ord`
  functions are partial (assume input is in the right byte-count
  bucket). Calling `chr3` on bytes that look like a 4-byte sequence
  produces a wrong `Char`. Documentation needs to spell this out.
* **Safety vs unsafety** — the `#`-suffixed primops bypass boxed
  representation. They're internal-only by convention. The
  convention isn't written down.

## Suggested fix

A pass over `Data.Unicode.hs` replacing each `TODO: docs` with a
one-or-two-line Haddock describing the function's contract.
Examples:

```haskell
-- | Encode a code point in @U+0080..U+07FF@ as a 2-byte UTF-8
-- sequence. Returns @(leader, continuation)@. Behavior is undefined
-- for inputs outside this range — callers must dispatch on
-- 'sizeofCharUtf8' first (or use 'writeUtf8OffPtr' which handles
-- dispatch).
--
-- @since 1.0.0
ord2 :: Char -> (Word8, Word8)
```

```haskell
-- | The width in bytes of the UTF-8 encoding of @c@. Returns 1, 2,
-- 3, or 4. Use with 'ord1'\/'ord2'\/'ord3'\/'ord4' to dispatch to
-- the right encoder.
--
-- @since 1.0.0
sizeofCharUtf8 :: Char -> Int
```

About 20 docstrings of this shape; one focused session of work.

The module-level Haddock should also be filled in with a brief
description of the package's role — a hand-rolled UTF-8 codec for
raw pointer arenas, distinct from `Data.Text.Encoding`'s
`ByteString`-based API.

The `.cabal` `synopsis` and `description` should similarly get real
text. These are visible on Hackage (if the package is ever
published) and in `cabal info`.

## Suggested ordering

Doc work is highest leverage *after* the correctness fixes
([[unicode-decoding-does-not-validate-utf8]],
[[unicode-readutf8offptr-no-bounds-check]]) land, because those
changes may alter function signatures (returning `Maybe`, taking
length args). Writing docs for the current shape and then changing
the shape would waste effort.

Order recommendation:

1. Fix decode-validation + bounds-check (changes signatures).
2. Resolve the duplication with `Opal.Common.Unicode`
   ([[unicode-duplication-with-opal-common-unicode]]).
3. *Then* document the consolidated module.
4. Optionally fill in `Data.Unicode.Prim`
   ([[unicode-prim-module-is-empty]]).
