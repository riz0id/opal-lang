# `Data.Unicode` is a near-duplicate of `Opal.Common.Unicode`

**Severity:** medium (maintenance hazard — divergent copies of the
same logic make every fix have to be applied in two places, and the
two copies have already drifted)

**Locations:**

* `packages/unicode/src/Data/Unicode.hs` (~249 lines)
* `packages/opal/src/Opal/Common/Unicode.hs` (~similar; the opal copy
  is the one actually used by the compiler)
* `packages/unicode/src/Data/Unicode/TH.hs` and
  `packages/opal/src/Opal/Common/TH.hs` (`staticListE`/`listToBytes`
  also duplicated)

## What the situation is

`packages/unicode/` exists as a standalone package. Its
`Data.Unicode` module implements UTF-8 encoding/decoding primitives —
`ord1`/`ord2`/`ord3`/`ord4`, `chr1`/`chr2`/`chr3`/`chr4`,
`readUtf8OffPtr`/`writeUtf8OffPtr`, `sizeofLeaderUtf8`,
`sizeofCharUtf8`, etc.

`packages/opal/src/Opal/Common/Unicode.hs` implements the **same**
primitives. The two files share the bulk of their logic verbatim,
but the opal copy has *additional* utilities that don't exist in
`Data.Unicode`:

* `copyStringUtf8ToPtr`
* `sizeofStringUtf8`
* `sizeofUtf8OffPtr`
* a handful of others

Search for callers:

```
$ grep -rn "import Data.Unicode" packages/opal/
(no results)
```

Nothing in `packages/opal/` imports `Data.Unicode`. The opal compiler
uses its own copy. `packages/unicode/` is **orphaned**.

The Template Haskell helpers are also duplicated:
`Data.Unicode.TH.staticListE` is a verbatim copy of
`Opal.Common.TH.staticListE` (the lookup table for UTF-8 leader
sizes).

## Why this matters

1. **Bug fixes diverge.** The decoder bugs in
   [[unicode-decoding-does-not-validate-utf8]] and
   [[unicode-readutf8offptr-no-bounds-check]] are present in *both*
   copies. Fixing one and not the other leaves a latent bug in the
   orphaned copy that will resurface the moment anyone integrates
   `Data.Unicode`.

2. **Feature drift.** The opal copy has extra utilities. If the
   `unicode` package is meant to be the canonical UTF-8 library,
   these utilities should live there and the opal copy should be a
   thin re-export. As-is, "where's the canonical place to put
   `sizeofStringUtf8`?" has two answers.

3. **The `unicode` package's purpose is ambiguous.** Was it carved
   out of opal to be a standalone library? Was it the original and
   opal copied from it? Was someone in the middle of migrating
   one direction or the other? Nothing in the codebase answers
   this.

## Suggested resolution

Pick one direction and execute:

### Option A: opal depends on `unicode` (preferred)

1. Move the extra utilities from `Opal.Common.Unicode` into
   `Data.Unicode` (or a sibling module like `Data.Unicode.String`).
2. Delete `Opal.Common.Unicode` and `Opal.Common.TH`.
3. Add `unicode` to opal.cabal's `build-depends`.
4. Update opal source to import from `Data.Unicode` / `Data.Unicode.TH`.

After this, the `unicode` package becomes load-bearing for opal and
gets exercised on every build.

### Option B: delete the `unicode` package

If there's no intent to make `unicode` standalone, delete the
package entirely (`packages/unicode/`) and remove it from
`cabal.project`. Closes this issue and reduces maintenance surface.

**Recommendation: A.** The `unicode` package's structure (its own
cabal, its own bench/test directories — currently empty, but
present) suggests the original intent was a standalone library. The
extra utilities can be migrated in a single commit.

## Out of scope

Fixing the two correctness bugs ([[unicode-decoding-does-not-validate-utf8]]
and [[unicode-readutf8offptr-no-bounds-check]]) before the
deduplication would mean applying each fix in two places. Better to
deduplicate first, then fix once.
