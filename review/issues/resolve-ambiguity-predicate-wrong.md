# `Opal.Resolve.ambiguous` uses the wrong predicate

**Severity:** critical (binding resolution can silently choose the wrong
binding, or report ambiguity where there is none)

**Location:** `packages/opal/src/Opal/Resolve.hs:72-73`, used at line 91.

## What the code says

```haskell
ambiguous :: ScopeSet -> ScopeSet -> Bool
ambiguous a b = not (a `ScopeSet.intersects` b)
```

…and then in `resolveId`:

```haskell
run b2@(Binding scps2 _) b1@(Binding scps1 _)
  | scps1 `ambiguous` scps2 = throwError (ResolveAmbiguous …)
  | otherwise               = pure (bestBinding b1 b2)
```

So two candidate bindings are reported as ambiguous **iff their scope sets
share no common scopes**.

## What the scope-sets model actually requires

From Flatt, *Bindings as Sets of Scopes*, §3 ("Resolving Bindings"):

> A reference resolves to the binding whose scope set is the largest subset
> of the reference's scope set. The reference is *ambiguous* if there are
> two candidate bindings whose scope sets are not comparable by the subset
> order — neither is a subset of the other.

The Racket expander encodes exactly this. In
`/Users/jake/Documents/Programming/Racket/racket/src/expander/syntax/scope.rkt`
around line 980-1010, `resolve` folds over candidates and at every step:

* if the new candidate's scope set is a (proper or equal) **subset** of the
  current best → keep the current best;
* if the new candidate's scope set is a **superset** of the current best →
  replace the best;
* **otherwise** ("neither subset" — i.e. incomparable) → flip into ambiguous
  mode (`(values (list best-scopes b-scopes) #f)`).

Two scope sets are *comparable* under ⊆ iff one is a subset of the other.
They are *incomparable* (hence ambiguous) iff neither subset relation holds.
That is **not** the same as "disjoint" — two scope sets can share scopes and
still be incomparable (e.g. `{a, b}` and `{a, c}`).

## Concrete failure modes

Consider an identifier `x` with scope set `S = {a, b, c}`, and a binding
store that contains:

* `B1 = {a, b}` → `x_1`
* `B2 = {a, c}` → `x_2`

Both are subsets of `S`, so `restrictBindings` returns both. They share `a`,
so they intersect, so Opal's `ambiguous` returns `False`. `resolveId` then
falls through to `bestBinding`:

```haskell
bestBinding b1@(Binding scps1 _) b2@(Binding scps2 _)
  | scps1 `ScopeSet.isSubsetOf` scps2 = b2
  | otherwise                         = b1
```

Neither is a subset of the other, so `isSubsetOf` returns `False`, and
`bestBinding` arbitrarily returns `b1`. Opal silently picks `x_1` — a
hygiene violation: it should have raised an ambiguity error per the paper
and per Racket.

Symmetrically, two disjoint scope sets that both happen to be subsets of
`S` (e.g. `B1 = {a}` and `B2 = {b}` against `S = {a, b}`) will trigger a
spurious ambiguity error even though Racket's resolver would simply pick
neither — neither subset chain is "larger" and they don't intersect, so by
Opal's logic they're ambiguous; by the paper they are also ambiguous, so
this case happens to coincide. The previous case (sharing some scope but
still incomparable) is where the bug bites.

## Suggested fix

Replace `ambiguous` with an "incomparable under ⊆" check, and let
`bestBinding` continue to handle the subset cases:

```haskell
incomparable :: ScopeSet -> ScopeSet -> Bool
incomparable a b =
  not (a `ScopeSet.isSubsetOf` b) && not (b `ScopeSet.isSubsetOf` a)
```

…and use that instead of `ambiguous` in `run`.

Better still, mirror Racket's accumulator: maintain either a "best so far"
or a *list* of incomparable scope sets, and only finalize ambiguity if no
later candidate is a superset of every member of the list. The current
pairwise fold can flag ambiguity prematurely if a third candidate would
have dominated both.

## Secondary smell

`Set.maxView canidates` (typo: `canidates` for `candidates`) picks the
lexicographically largest scope set by the derived `Ord` on `ScopeSet`,
which is determined by the underlying `Set Scope`'s `Ord` — i.e. by the
internal `Word` ids of the scopes. That ordering has no semantic
relationship to subset-largeness, so the "starting point" of the fold is
arbitrary. This is masked by `bestBinding` swapping in supersets as they
appear, but it does mean the fold's behavior depends on `Scope` allocation
order rather than on the scope-set structure. Initializing with `Nothing`
and inserting as in Racket's `for*/fold` would be clearer and order-
independent.
