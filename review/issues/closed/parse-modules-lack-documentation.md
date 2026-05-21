# Every `Text.Parse.*` module header has `TODO: docs`

**Severity:** low (won't bite anyone, but every contributor who
reaches for this library has to reverse-engineer it from
implementation)

**Locations:**

* `packages/parse/src/Text/Parse.hs:12`
* `packages/parse/src/Text/Parse/Buffer.hs` (module header)
* `packages/parse/src/Text/Parse/Error.hs:12`
* `packages/parse/src/Text/Parse/Monad.hs` (module header)
* `packages/parse/src/Text/Parse/State.hs` (module header)
* `packages/parse/src/Text/Parse/Token.hs` (module header)

Every individual function within these modules is also annotated
`-- | TODO: docs` instead of a real Haddock description.

## What's missing

Six modules' worth of API surface (`Buffer`, `State`, `Error`,
`Monad`, `Token`, and the public `Text.Parse`) with no narrative
explaining:

* **What `Text.Parse` is for** — it's a custom hand-rolled parser
  combinator library; not a Megaparsec wrapper. A reader who lands
  in `Text.Parse` from a grep would not know whether to expect
  Megaparsec-style behaviour.
* **What `Buffer`'s memory model is** — `ForeignPtr Word8` pinned
  memory, intentional choice for raw-pointer ops. Without a
  rationale, a future contributor might propose replacing it with
  `ByteString` not realising the design choice.
* **Error model** — `ParseError` accumulates `(expected, received)`
  token sets. Different from Megaparsec; needs explaining when a
  developer compares the two.
* **How to extend** — the combinator set is minimal (see
  [[parse-combinator-library-incomplete]]); a doc note would point
  contributors at the right entry point for adding `many`, `choice`,
  etc.
* **Streaming caveat** — the buffer is all-in-memory (see
  [[parse-no-streaming-full-input-required]]). Worth flagging at the
  top of `Text.Parse.Buffer` so users don't try to feed it 4 GB
  files.

## Suggested fix

Two passes:

1. **Module headers.** For each module, replace the `TODO: docs`
   with a one-paragraph description matching the existing
   `Opal.*` modules' style. Example for `Text.Parse`:

   ```haskell
   -- | The public combinator surface for `Text.Parse`, a hand-rolled
   --   in-memory parser-combinator library built on pinned-memory
   --   buffers. Distinct from `Opal.Reader`'s Megaparsec-based
   --   reader. Currently exposes only `consume`, `single`, `string`,
   --   and `try`; richer combinators land in
   --   `review/issues/open/parse-combinator-library-incomplete.md`.
   ```

2. **Function-level Haddock.** Replace each `TODO: docs` on individual
   bindings with a sentence describing intent + invariants. Don't
   need novel-length; one or two lines each is enough.

Not blocking any other work, but each open `parse` issue (see
cross-references) becomes easier to act on once the docs explain the
intent. Worth doing alongside the substantive `parse` work
(`takeBuffer` fix + combinator zoo), not as a standalone commit.

## Out of scope

A full README for the package itself. That's a separate artifact;
this issue is just about Haddock module headers and function-level
docs.
