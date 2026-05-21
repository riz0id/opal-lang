# `Data.Unicode.chr{2,3,4}` decode without validating UTF-8 structure

**Severity:** high (silently produces invalid Unicode scalar values
from malformed input — bug-class is "garbage in, plausible garbage
out," very hard to debug)

**Location:** `packages/unicode/src/Data/Unicode.hs:160-167` (`chr4#`),
plus the analogous `chr2#`/`chr3#` definitions nearby.

## What the code says

```haskell
chr4# :: Word8# -> Word8# -> Word8# -> Word8# -> Char#
chr4# x# y# z# w# =
  let !b0# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# x#) 0x0f##) 18#
      !b1# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# y#) 0x7f##) 12#
      !b2# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# z#) 0x7f##) 6#
      !b3# = GHC.uncheckedShiftL# (GHC.and# (GHC.word8ToWord# w#) 0x7f##) 0#
   in wordToChar# (b0# `GHC.or#` b1# `GHC.or#` b2# `GHC.or#` b3#)
```

The decoder takes the leader byte and three continuation bytes, masks
each, and ORs them into a 21-bit code point. Two correctness gaps:

1. **No validation of continuation bytes.** A valid UTF-8
   continuation byte has the high bits `10xxxxxx`. The decoder
   simply masks with `0x7f` and uses whatever the lower 6 bits are,
   regardless of whether the high bits are correct. A malformed
   input like `0xF0 0xC0 0x80 0x80` (where `0xC0` is *not* a valid
   continuation — its top bits are `11`) decodes to a code point
   that looks legitimate but came from invalid UTF-8.

2. **No range check on the result.** The four-byte form encodes
   code points in `U+10000..U+10FFFF`. A malformed input can produce
   code points in:
   * The overlong range `U+0000..U+FFFF` (forbidden — should be
     encoded with fewer bytes).
   * The surrogate range `U+D800..U+DFFF` (forbidden in UTF-8).
   * The out-of-Unicode range `U+110000..U+1FFFFF`.

   The decoder uses `wordToChar#` directly, which constructs a
   `Char` from a `Word` with no validation. Producing a `Char` in
   the surrogate range or above `U+10FFFF` violates Haskell's `Char`
   contract (it's supposed to be a valid Unicode scalar).

The same shape applies to `chr2#` and `chr3#` — only `chr1#` (the
1-byte ASCII path) is necessarily safe because the input range is
constrained by the leader-byte check.

## Failure modes

* **Silent invalid `Char` production.** Code consuming the decoder
  output assumes valid Unicode scalars. A `Char` in the surrogate
  range that flows into, say, `Text` construction or JSON encoding
  will produce invalid output or crash a downstream consumer that
  *does* validate.

* **Overlong encoding acceptance.** A classic security concern:
  attackers smuggle "safe" bytes through filters by encoding them
  overlong. E.g. `'/'` as a 2-byte sequence `0xC0 0xAF` instead of
  the 1-byte `0x2F`. A naive path-traversal filter checking for
  literal `/` bytes would be bypassed. (The opal compiler isn't a
  security boundary today, but the decoder is library code that may
  end up there.)

## What it should do

Validate each continuation byte's top bits before masking:

```haskell
chr4 :: Word8 -> Word8 -> Word8 -> Word8 -> Maybe Char
chr4 x y z w
  | not (isContinuation y && isContinuation z && isContinuation w)
      = Nothing
  | codePoint < 0x10000      = Nothing  -- overlong
  | codePoint > 0x10FFFF     = Nothing  -- out of range
  | codePoint >= 0xD800
    && codePoint <= 0xDFFF   = Nothing  -- surrogate
  | otherwise                = Just (chr codePoint)
  where
    codePoint = … the existing OR-mask logic …
    isContinuation b = b .&. 0xC0 == 0x80
```

Caller decides what to do with `Nothing` — error, replacement
character U+FFFD, etc.

## Why this hasn't bitten yet

The `unicode` package is orphaned. `Opal.Common.Unicode` has the
same decoder logic and is used by the opal compiler, but only on
input from well-formed source files (the reader handles UTF-8 via
Megaparsec which validates). The bug activates the moment either
module decodes externally-sourced bytes that aren't pre-validated.

## Suggested fix

Two passes:

1. **Wrap the `#`-suffixed primops** (`chr2#`, `chr3#`, `chr4#`) in
   validating boxed versions (`Maybe Char`-returning). Keep the
   `#` versions as the fast unchecked path, but document them as
   internal.

2. **Update `readUtf8OffPtr`** (`Data.Unicode.hs:172-189`) to use
   the validating versions and surface decode failures in its
   return type — e.g., `IO (Either DecodeError (Char, Int))` or
   `IO (Maybe (Char, Int))`.

## Cross-reference

This bug compounds with
[[unicode-readutf8offptr-no-bounds-check]] — both are safety gaps in
the UTF-8 decoder path. They should be fixed together (and the same
fix applied to the duplicate `Opal.Common.Unicode` per
[[unicode-duplication-with-opal-common-unicode]]).
