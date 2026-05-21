# `Opal.Module.moduleDefinitions` is `undefined`

**Severity:** medium (latent crash bomb; the lens is exported and will
bottom on any read or write)

**Location:** `packages/opal/src/Opal/Module.hs:326-327`

## What the code says

```haskell
-- | Composite lens focusing on the @('moduleNamespace' . 'namespaceDefinitions')@
-- field of a 'Module'.
--
-- @since 1.0.0
moduleDefinitions :: Lens' Module [(PhaseShift, Definition)]
moduleDefinitions = moduleNamespace . undefined
```

The body uses `undefined` for the second component of the lens
composition. Any forced use (e.g., `view moduleDefinitions m`,
`set moduleDefinitions xs m`, `over moduleDefinitions f m`) will
evaluate the `undefined` and throw
`Prelude.undefined`.

The function is exported (`Opal.Module` re-exports `moduleDefinitions`
in its module header) and is part of the public API surface. Anything
that reaches for it — whether downstream code, future expander passes,
or test code — will hit the bottom.

It also doesn't typecheck against its claimed signature in a
meaningful way: `moduleNamespace :: Lens' Module Namespace`, but no
`Namespace` lens of type `Lens' Namespace [(PhaseShift, Definition)]`
exists today. The closest existing field is `Namespace.ns_phases ::
Map Phase Definitions`, which is structurally different (a `Map`
keyed by phase, not a list of phase-shift/definition pairs).

## What this should look like

Two plausible repairs depending on what the intended return type is:

1. **If a `[(PhaseShift, Definition)]` flattening is wanted** —
   construct it from `Namespace`'s `ns_phases`:

   ```haskell
   moduleDefinitions :: Getter Module [(Phase, Definition)]
   moduleDefinitions = moduleNamespace . to flatten
     where
       flatten ns =
         [ (ph, d)
         | (ph, defns) <- Map.toList (ns ^. nsPhases)
         , d           <- definitionsToList defns
         ]
   ```

   …but note that `[(Phase, _)]` is the natural pair, not
   `[(PhaseShift, _)]`. The signature's choice of `PhaseShift` looks
   wrong: definitions live at a phase, not a phase-shift; phase-shifts
   are an import-relative offset (cf. `Module.Import` /
   `Module.Export`).

   This can only be a `Getter`, not a `Lens'`, because the flattening
   is lossy (you can't recover the `Map Phase Definitions` shape from
   a list of pairs without extra structure).

2. **If a direct lens onto the `Map`-shaped store is wanted** —
   compose with `nsPhases`:

   ```haskell
   moduleDefinitions :: Lens' Module (Map Phase Definitions)
   moduleDefinitions = moduleNamespace . nsPhases
   ```

   This *is* a real `Lens'` (round-trips), and matches the existing
   shape of `Namespace`. Callers wanting a flat list can `Map.toList`
   themselves.

Option (2) is the lower-risk change and matches the existing
`Namespace` API.

## Suggested action

Either delete the export and the definition (it has no callers
currently — `grep moduleDefinitions` returns only the definition
itself), or replace with option (2). Leaving `undefined` in the source
of an exported binding is a footgun: it compiles, it typechecks, it
passes any test that doesn't touch it, and it bottoms hard when
anyone reaches for it.
