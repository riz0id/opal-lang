# `MultiScope.delete Nothing` / `MultiScope.deletes Nothing` discard the entire `MultiScope`

**Severity:** critical (every `ScopeInfo.insert Nothing` and every
`ScopeInfo.union` silently erases all per-phase scopes; the expander
calls these on essentially every syntax object it touches)

**Locations:**

* `packages/opal/src/Opal/Common/MultiScope.hs:160-169` — `delete`
* `packages/opal/src/Opal/Common/MultiScope.hs:174-185` — `deletes`
* Reachable from `Opal/Syntax/ScopeInfo.hs:113, 127, 163, 164`

## What the code says

```haskell
delete :: Maybe Phase -> Scope -> MultiScope -> MultiScope
delete mph sc mscp = case mph of
  Nothing -> foldr (\ph -> delete (Just ph) sc) empty (phases mscp)
  Just ph -> alter ph (>>= update) mscp
  where ...

deletes :: Maybe Phase -> ScopeSet -> MultiScope -> MultiScope
deletes mph scps mscp
  | ScopeSet.null scps = mscp
  | otherwise          = case mph of
    Nothing -> foldr (\ph -> deletes (Just ph) scps) empty (phases mscp)
    Just ph -> alter ph (>>= update) mscp
  where ...
```

The `Nothing` branch is supposed to "delete `sc` from *every* phase in
`mscp`". The implementation folds `delete (Just ph) sc` over the set of
phases — but it folds onto the **`empty` accumulator** instead of onto
`mscp`.

Trace it with a concrete example. Let `mscp` have entries at phases
`{0, 1}`:

```
foldr (\ph -> delete (Just ph) sc) empty {0, 1}
= delete (Just 0) sc (delete (Just 1) sc empty)
= delete (Just 0) sc empty                          -- nothing at phase 1 in empty
= empty                                              -- nothing at phase 0 in empty
```

So `delete Nothing _ _` and `deletes Nothing _ _` return `empty` for
**any** input `MultiScope`. The phase-set is consulted only to determine
which fold steps to do; the input scope sets are never threaded through.

The intended code is the standard "iterate over phases, threading the
accumulator":

```haskell
Nothing -> foldr (\ph -> delete (Just ph) sc) mscp (phases mscp)
```

(start from `mscp`, not `empty`) — and analogously for `deletes`.

## Why this matters

These functions are the *only* deletion path used by `ScopeInfo` when a
phase-independent (`Nothing`) scope is added or when two `ScopeInfo`s
are unioned. Every such call now silently wipes the entire per-phase
`MultiScope`.

### Reachable callers

1. `ScopeInfo.insert Nothing sc info` —
   `packages/opal/src/Opal/Syntax/ScopeInfo.hs:111-114`:

   ```haskell
   insert Nothing sc (ScopeInfo gscps mscps) =
     let gscps' = ScopeSet.insert sc gscps
         mscps' = MultiScope.delete Nothing sc mscps  -- always returns empty
      in ScopeInfo gscps' mscps'
   ```

   Adding a single phase-independent scope to a `ScopeInfo` *erases all
   of its per-phase scopes*. Every `syntaxScope Nothing sc s` (called
   from `runExpandSyntax` on the top-level form, from `expandImport`,
   from `moduleToSyntax`, from `coreFormIdentifier`, from `exportToSyntax`,
   etc.) hits this path.

2. `ScopeInfo.inserts Nothing scps info` —
   `packages/opal/src/Opal/Syntax/ScopeInfo.hs:123-128`: same problem
   for sets of scopes.

3. `ScopeInfo.union` —
   `packages/opal/src/Opal/Syntax/ScopeInfo.hs:160-165`:

   ```haskell
   union (ScopeInfo gscps1 mscps1) (ScopeInfo gscps2 mscps2) =
     let gscps   = ScopeSet.union gscps1 gscps2
         mscps1' = MultiScope.deletes Nothing gscps2 mscps1  -- ⇒ empty
         mscps2' = MultiScope.deletes Nothing gscps1 mscps2  -- ⇒ empty
      in ScopeInfo gscps (mscps1' <> mscps2')                 -- ⇒ empty
   ```

   Because both `mscps1'` and `mscps2'` are forced to `empty`, the
   union of any two `ScopeInfo`s has `MultiScope.empty` for its
   per-phase component. `ScopeInfo`'s `Semigroup`/`Monoid` instance is
   the same operation, so `info1 <> info2` also loses per-phase
   scopes.

### Concrete failure mode

Take a syntax object `s` that already has a phase-1 scope `sc1` (e.g.,
attached by a `define-syntax` transformer body). Apply *any*
phase-independent scope to it — for instance, the default
`runExpandSyntax` opening line:

```haskell
let stx' = syntaxScope Nothing def stx
```

After this single call, `stx'` has `def` in its global scope set and
**nothing** in its per-phase store. The previously-attached phase-1
scope `sc1` is gone. Subsequent `resolve (Just 1) id` will not see
`sc1`, and any binding that was supposed to be visible at phase 1 via
`sc1` becomes unresolvable.

### Why it isn't yet visible

In current code, per-phase scopes are only ever inserted via the
expander's `flipSyntax` / `scopeSyntax True` paths, which run at the
current `expandCurrentPhase`. Since the existing test cases mostly run
at phase 0 only, and `expandCurrentPhase` is 0 throughout, per-phase
slots tend to be empty at the moment the deletion happens — so the bug
"deletes empty, leaves empty" and looks harmless.

As soon as multi-phase expansion is exercised (a `define-syntax` whose
RHS itself uses a macro at phase 1), per-phase scope sets become
non-empty and this bug starts silently corrupting them.

## Suggested fix

Two lines:

```haskell
delete mph sc mscp = case mph of
- Nothing -> foldr (\ph -> delete (Just ph) sc) empty   (phases mscp)
+ Nothing -> foldr (\ph -> delete (Just ph) sc) mscp    (phases mscp)
  Just ph -> alter ph (>>= update) mscp

deletes mph scps mscp
  | ScopeSet.null scps = mscp
  | otherwise          = case mph of
-   Nothing -> foldr (\ph -> deletes (Just ph) scps) empty (phases mscp)
+   Nothing -> foldr (\ph -> deletes (Just ph) scps) mscp  (phases mscp)
    Just ph -> alter ph (>>= update) mscp
```

A QuickCheck-shaped property worth adding:

```
MultiScope.delete Nothing sc m
  == foldr (\ph -> MultiScope.delete (Just ph) sc) m (MultiScope.phases m)
```

which currently fails for every non-empty `m`.

## Cross-reference

This bug masks the visible symptoms of
[[scope-info-inserts-intersection-bug]] and
[[intro-and-use-site-scopes-are-phase-specific]]: as long as the
per-phase `MultiScope` is wiped on every phase-independent insert, the
broken phase-specific manipulations downstream don't matter because
the per-phase state isn't preserved across operations anyway. Fixing
this issue is a prerequisite to surfacing the other phase-related
bugs.
