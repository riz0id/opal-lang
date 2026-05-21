# `Text.Parse.Buffer.takeBuffer` keeps the *wrong* slice

**Severity:** critical (silent data corruption: callers asking for the
first @n@ bytes of a buffer get back a slice of the wrong length, with
the wrong content, no error raised)

**Location:** `packages/parse/src/Text/Parse/Buffer.hs:271-274`

## What the code says

```haskell
takeBuffer :: Int -> Buffer -> Buffer
takeBuffer n (Buffer fp len)
  | n < len   = Buffer fp (len - n)
  | otherwise = emptyBuffer
```

`takeBuffer n` is meant (by name parallel to `Data.List.take`) to
return the **first @n@ bytes** of the buffer. But the result is
`Buffer fp (len - n)` — it keeps the original pointer but sets the
length to `len - n`. So:

* The returned buffer still points at the *start* of the original
  buffer's memory.
* Its length is `len - n` instead of `n`.

For a 100-byte buffer and `takeBuffer 10`:
* **Expected:** a buffer of length 10 containing bytes 0..9.
* **Actual:** a buffer of length 90 containing bytes 0..89.

So `takeBuffer` returns *most* of the buffer when asked for a *small*
prefix, and vice versa. Compare to the sibling `dropBuffer`
immediately above:

```haskell
dropBuffer :: Int -> Buffer -> Buffer
dropBuffer n (Buffer fp len)
  | n < len   = Buffer (fp `plusForeignPtr` n) (len - n)
  | otherwise = emptyBuffer
```

`dropBuffer` correctly advances the pointer by `n` and shortens the
length by `n` — giving the *suffix*. `takeBuffer` was clearly meant
to mirror this for the *prefix* but the formula was carried over
unchanged.

## What it should be

```haskell
takeBuffer :: Int -> Buffer -> Buffer
takeBuffer n (Buffer fp len)
  | n < len   = Buffer fp n
  | otherwise = Buffer fp len   -- or just `Buffer fp len` if n >= len
```

i.e., keep the same pointer, set the length to exactly `n`.

The `otherwise` branch (currently `emptyBuffer`) is also worth
revisiting: if `n >= len`, the caller asked for at least the whole
buffer, so the natural answer is the whole buffer, not an empty one.

## Why it isn't yet causing crashes

`takeBuffer` doesn't appear to have any callers in `Text.Parse`'s
exposed API (`Parse.hs`'s public combinators don't reach it).
Internal callers within `Buffer.hs` may or may not — needs an audit.
Currently the package is orphaned from `opal` (no imports from
`packages/opal/`), so the bug isn't reachable from real use yet.

It will activate the moment any consumer uses `takeBuffer` for
buffer-slicing — most likely use cases include token capture (capture
the first `n` characters of input as a lexeme) and lookahead buffers.

## Suggested fix

Two-character change on line 273:

```diff
- | n < len   = Buffer fp (len - n)
+ | n < len   = Buffer fp n
```

Optionally also widen the `otherwise` clause to return the original
buffer when `n >= len`, matching `Data.List.take` semantics.

## Regression test (recommended)

Once `parse` is wired into the test suite (it isn't today), add:

```haskell
takeBuffer 3 (bufferOfString "hello") `bufferEqualsBytes` "hel"
takeBuffer 0 (bufferOfString "hello") `bufferEqualsBytes` ""
takeBuffer 100 (bufferOfString "hi")  `bufferEqualsBytes` "hi"
```
