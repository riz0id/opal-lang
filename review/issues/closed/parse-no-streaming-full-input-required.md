# `Text.Parse` requires the full input upfront — no streaming

**Severity:** medium (limits usable input sizes and rules out
interactive/REPL parsing; a structural design concern, not a bug
per se)

**Location:**
* `packages/parse/src/Text/Parse/Monad.hs` (the `parse` top-level
  runner allocates a buffer from a `String`).
* `packages/parse/src/Text/Parse/Buffer.hs` (`Buffer` is a fixed-size
  pinned-memory region, not a chunked stream).

## What the code does

The state carried by `Parse` is:

```haskell
data ParseState = ParseState
  { parse_state_buffer :: !Buffer
  , parse_state_offset :: !Int
  }
```

…where `Buffer` is a `ForeignPtr Word8` of a fixed length. The
top-level runner allocates the full buffer from a `String` input
upfront. Once parsing starts, there is no mechanism to extend the
buffer or stream from a source. EOF is determined by the offset
reaching the buffer's length.

This means:

1. **No incremental parsing.** A parser cannot consume input as it
   arrives over a handle, socket, or interactive prompt. The full
   contents must be materialised before `parse` is called.

2. **Memory pressure on large inputs.** A 1 GB source file requires
   1 GB of pinned memory before parsing begins. Megaparsec (the
   current `Opal.Reader` backend) supports `ByteString` and `Text`
   inputs that can be lazy.

3. **No "resume after partial parse" support.** A typical REPL
   workflow — parse one form, return to the prompt, parse the next —
   doesn't fit; each new call re-allocates a buffer.

## What's needed for streaming

To get incremental parsing, two layers need to change:

1. **Buffer becomes chunked.** Either:
   * A linked list of fixed-size chunks with a current-chunk pointer
     in `ParseState`; or
   * A reader callback in `ParseState` (`IO (Maybe ByteString)`) that
     `consume` calls when the current chunk is exhausted.

2. **`consumeParseState` learns to refill.** When the byte offset
   reaches the end of the current chunk, it requests the next.
   Returns `Nothing` only when the source signals true EOF.

3. **Backtracking interacts with chunk eviction.** `try` saves a
   `ParseState`; restoring it must keep the chunk reachable. A
   reference-counted chunk model or a "no eviction past the oldest
   live save-point" rule.

This is a meaningful redesign — not a small patch.

## Why this is medium (not high) severity

The package is orphaned today; no consumer is hitting the limit.
`Opal.Reader` (Megaparsec) handles the project's current parsing
needs, and Megaparsec's `Text` input is good enough for
foreseeable file sizes in the Opal compiler.

The streaming concern only matters if `Text.Parse` is meant to
*replace* `Opal.Reader` — in which case Megaparsec's streaming
support is a baseline to match.

## Suggested action

Either:

1. **Defer.** Document the limitation in the module Haddock (see
   [[parse-modules-lack-documentation]]) and treat `Text.Parse` as
   a "for in-memory strings only" library.

2. **Plan a chunked-buffer redesign.** Probably its own dedicated
   plan document under `plans/`; not appropriate for a single
   commit.

Recommendation: **defer** until the question "is `Text.Parse`
replacing `Opal.Reader`?" is answered. If yes, planning the chunked
buffer is one of the first design tasks. If no, this issue can be
closed as won't-fix.

## Resolution (deferred)

Closed as **deferred**, not fixed. The `parse` package remains
orphaned from `opal`; until there's a concrete consumer that needs
streaming, in-memory is acceptable. Reopen this issue (or file a new
one) the moment `parse` gets a real caller that needs to handle
inputs that don't fit comfortably in memory.
