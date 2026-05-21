# `packages/unicode/` has no test coverage

**Severity:** medium (the package implements correctness-sensitive
UTF-8 codec code with zero test exercise; bugs like
[[unicode-decoding-does-not-validate-utf8]] would be caught
immediately by even minimal tests)

**Location:** `packages/unicode/test/` (empty), `packages/unicode/bench/`
(empty), `packages/unicode/unicode.cabal` (no `test-suite` stanza)

## What's there

The package's `test/` and `bench/` directories exist but contain no
`.hs` files. The `.cabal` declares no `test-suite` or `benchmark`
stanza. Running `cabal test unicode` finds nothing to do.

## What should be tested

UTF-8 has notoriously subtle correctness corners — exactly the
shape of code where lazy testing produces real bugs. Minimum
coverage:

### Encoding (`ord1`/`ord2`/`ord3`/`ord4`)

* Round-trip property: `chr (encode c)` ≡ `Just c` for every valid
  `Char`. Hedgehog generator over `Gen.unicode` for ~100 trials
  catches encoding-byte-order mistakes.
* Boundary code points:
  * `U+007F` (last 1-byte form) encodes to a single byte.
  * `U+0080` (first 2-byte) encodes to two bytes with leader
    `0xC2`.
  * `U+07FF` (last 2-byte) → `0xDF 0xBF`.
  * `U+0800` (first 3-byte) → `0xE0 0xA0 0x80`.
  * `U+FFFF` (last 3-byte) → `0xEF 0xBF 0xBF`.
  * `U+10000` (first 4-byte) → `0xF0 0x90 0x80 0x80`.
  * `U+10FFFF` (last) → `0xF4 0x8F 0xBF 0xBF`.

### Decoding (`chr1`/`chr2`/`chr3`/`chr4`)

* Round-trip with encoding (above).
* Reject malformed continuation bytes (see
  [[unicode-decoding-does-not-validate-utf8]]).
* Reject overlong encodings (`0xC0 0x80`, etc.).
* Reject surrogates (`0xED 0xA0 0x80` decodes to a U+D800 surrogate).

### Pointer ops (`readUtf8OffPtr`/`writeUtf8OffPtr`)

* `writeUtf8OffPtr p c` then `readUtf8OffPtr p` round-trips.
* Bounds-check failure mode (once the bounds-check from
  [[unicode-readutf8offptr-no-bounds-check]] is added):
  `readUtf8OffPtr p 1` on a buffer with a leader byte requesting 2
  bytes returns `Nothing` rather than reading OOB.

### `sizeofLeaderUtf8` / `sizeofCharUtf8`

* Every leader byte 0x00..0xFF: lookup matches the spec.
* `sizeofCharUtf8` agrees with the actual encoding length.

## Suggested setup

A test-suite stanza in `unicode.cabal` mirroring `parse`'s shape
(once `parse` gets one) — `tasty` + `hedgehog` is the existing
convention from `packages/opal/test/`:

```cabal
test-suite unicode-test
  import:         common
  type:           exitcode-stdio-1.0
  hs-source-dirs: test
  main-is:        Test.hs
  build-depends:
    , unicode
    , hedgehog
    , tasty
    , tasty-hedgehog
  other-modules:
    Test.Data.Unicode
```

With `Test.Data.Unicode` exporting a `testTree :: TestTree` per the
convention.

~50 lines of test for the round-trip + boundary cases would catch
the two correctness issues filed alongside this one
([[unicode-decoding-does-not-validate-utf8]],
[[unicode-readutf8offptr-no-bounds-check]]) and provide a baseline
for future changes.

## Aside

The `bench/` directory is similarly empty. Bench targets for
`readUtf8OffPtr` / `writeUtf8OffPtr` are valuable since the package
exists specifically for performance (raw pointer ops over GHC
primitives), and unbenchmarked perf code is hard to keep optimized.
Worth its own follow-up, but lower priority than tests.
