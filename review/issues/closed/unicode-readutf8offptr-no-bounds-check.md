# `Data.Unicode.readUtf8OffPtr` performs unbounded reads

**Severity:** high (out-of-bounds reads on near-end-of-buffer
input — can crash with SIGSEGV or silently return garbage)

**Location:** `packages/unicode/src/Data/Unicode.hs:172-189`

## What the code says

```haskell
readUtf8OffPtr :: Ptr Word8 -> IO (Char, Int)
readUtf8OffPtr ptr = do
  cu1 <- readWord8OffPtr ptr 0
  case sizeofLeaderUtf8 cu1 of
    4 -> do
      cu2 <- readWord8OffPtr ptr 1
      cu3 <- readWord8OffPtr ptr 2
      cu4 <- readWord8OffPtr ptr 3
      pure (chr4 cu1 cu2 cu3 cu4, 4)
    3 -> do
      cu2 <- readWord8OffPtr ptr 1
      cu3 <- readWord8OffPtr ptr 2
      pure (chr3 cu1 cu2 cu3, 3)
    2 -> do
      cu2 <- readWord8OffPtr ptr 1
      pure (chr2 cu1 cu2, 2)
    _ ->
      pure (chr1 cu1, 1)
```

The function takes a `Ptr Word8` and reads 1–4 bytes from it
depending on the leader byte's UTF-8 sequence length. **There is no
length parameter or bounds check.** Calling
`readUtf8OffPtr p` where `p` is a pointer to the last byte of a
buffer, and that last byte is a 4-byte UTF-8 leader (`0xF0..0xF4`),
will read **three bytes past the end of the buffer**.

This is a memory-safety bug in any context where `ptr` doesn't point
into an arena that's guaranteed to extend at least 4 bytes past the
buffer's logical end (most callers won't have that guarantee).

## Failure modes

1. **Crash (SIGSEGV).** If the buffer ends near a page boundary and
   the next page isn't mapped, the read faults.
2. **Silent garbage.** More likely: the read succeeds but returns
   whatever bytes happen to be in adjacent memory — heap, stack,
   another buffer. The resulting `Char` is a plausible-looking but
   completely wrong Unicode code point.
3. **Information disclosure.** In adversarial contexts (untrusted
   input feeding a parser using this function), an attacker who
   controls byte sequences can probe out-of-bounds memory by
   constructing inputs that end with multi-byte leaders.

The companion `writeUtf8OffPtr` (lines 195-218) has the same shape
on the write side — no bounds check, writes 1–4 bytes from the
pointer. Same severity.

## What it should do

Take a length and refuse to read past it. Two API options:

### Option A: explicit length argument

```haskell
readUtf8OffPtr :: Ptr Word8 -> Int -> IO (Maybe (Char, Int))
readUtf8OffPtr ptr remaining = do
  cu1 <- readWord8OffPtr ptr 0
  let needed = sizeofLeaderUtf8 cu1
  if remaining < needed
    then pure Nothing
    else case needed of
      4 -> …
      …
```

Returns `Nothing` when the leader claims a sequence that doesn't
fit. Caller decides whether to error, replace with U+FFFD, etc.

### Option B: length encoded in a buffer type

If the package adopts a `Buffer = (Ptr Word8, Int)` shape (like
`packages/parse/src/Text/Parse/Buffer.hs`), the bounds check is
implicit:

```haskell
readUtf8 :: Buffer -> Int -> IO (Maybe (Char, Int))
readUtf8 (Buffer fp len) offset = …
```

Either form is acceptable; (A) is the smaller change.

## Why this hasn't bitten yet

The `unicode` package is orphaned — no callers in
`packages/opal/`. The opal compiler uses its own
`Opal.Common.Unicode` (a near-duplicate of this module, see
[[unicode-duplication-with-opal-common-unicode]]). Whichever module
gets integrated first will inherit this bug.

## Suggested fix

Adopt option (A): add the `Int` length parameter and return
`Maybe`. Update `Opal.Common.Unicode`'s parallel function the same
way when the duplication is resolved.

A small unit test, once the test suite is set up (see
[[unicode-no-test-coverage]]), should cover:

* Reading at exact buffer end (1-byte leader → ok; 2-byte leader →
  `Nothing`).
* Reading 1 byte before buffer end with a 4-byte leader → `Nothing`.
* Reading a truncated multi-byte leader.
