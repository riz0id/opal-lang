# `Opal.Syntax.ScopeInfo.inserts` uses `intersection` where `difference` is intended

**Severity:** high (silently drops scopes when adding phase-specific scope
sets — corrupts identifier scope sets at non-base phases)

**Location:** `packages/opal/src/Opal/Syntax/ScopeInfo.hs:129-136`.

## What the code says

```haskell
inserts (Just ph) scps (ScopeInfo gscps mscps)
  | ScopeSet.null scps = ScopeInfo gscps mscps
  | otherwise =
    let scps' :: ScopeSet
        scps' = ScopeSet.intersection scps gscps
     in if ScopeSet.null scps'
          then ScopeInfo gscps mscps
          else ScopeInfo gscps (MultiScope.inserts ph scps' mscps)
```

In English, `inserts (Just ph) scps info` is supposed to add the set
`scps` of scopes to the per-phase store at phase `ph`. The current
implementation:

1. Computes `scps' = scps ∩ gscps` — i.e. *only the scopes that are already
   in the phase-independent set*.
2. If `scps'` is empty, leaves `info` unchanged.
3. Otherwise, inserts `scps'` into the phase-`ph` slot of the multi-scope.

So a fresh scope that should go into phase `ph` is silently dropped (it
isn't in `gscps`, so it's not in the intersection). The only scopes ever
written to the per-phase store are ones that are *already* phase-
independent — which makes the per-phase entry strictly redundant with
`gscps`, never adding new information.

## What it should do

Compare to the singular `insert (Just ph) sc` on the line above:

```haskell
insert (Just ph) sc (ScopeInfo gscps mscps)
  | ScopeSet.member sc gscps = ScopeInfo gscps mscps
  | otherwise = ScopeInfo gscps (MultiScope.insert ph sc mscps)
```

That implements the correct invariant: "if `sc` is already global, do
nothing; otherwise add `sc` to the per-phase set." Generalized to a set,
the correct implementation of `inserts (Just ph) scps` is:

```haskell
inserts (Just ph) scps (ScopeInfo gscps mscps)
  | ScopeSet.null scps = ScopeInfo gscps mscps
  | otherwise =
    let scps' = ScopeSet.difference scps gscps   -- the scopes NOT already global
     in if ScopeSet.null scps'
          then ScopeInfo gscps mscps
          else ScopeInfo gscps (MultiScope.inserts ph scps' mscps)
```

i.e. `difference`, not `intersection`. The implementation almost certainly
started from `insert`'s logic and then got `intersection` instead of
`difference` (likely typo — they're the dual operations on `Data.Set`).

## Why it isn't yet causing visible breakage

`inserts (Just ph)` is not called directly from anywhere in the source
tree (`grep ScopeInfo.inserts` returns only the definition). It is
reachable only through the `Ixed ScopeInfo` instance:

```haskell
instance Ixed ScopeInfo where
  ix ph f info = fmap (\x -> inserts ph x info) (f (lookup ph info))
```

so any code that does `info & ix (Just ph) .~ scps` or similar will hit
the bug. None exists today, but the bug will activate the first time a
non-trivial phase-specific scope set is written via the lens — i.e. as
soon as the expander begins manipulating phase-1 (compile-time) scopes
for `define-syntax`/`begin-for-syntax` properly.

## Suggested fix

Replace `intersection` with `difference` in
`Opal/Syntax/ScopeInfo.hs:133`. Add a small unit test (or a quickcheck
property) asserting:

```
ScopeInfo.lookup (Just ph) (ScopeInfo.inserts (Just ph) scps info)
  == ScopeInfo.lookup (Just ph) info `union` scps
```

— which currently fails for every `scps` containing scopes not in `gscps`.
