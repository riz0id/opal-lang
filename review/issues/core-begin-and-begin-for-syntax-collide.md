# `CoreBegin` and `CoreBeginSyntax` collide on the symbol `begin`

**Severity:** high (`begin-for-syntax` is unreachable; any source-level
use of it parses as `begin`, silently changing semantics)

**Locations:**

* `packages/opal/src/Opal/Core.hs:103-115` — `coreFormString`
* `packages/opal/src/Opal/Binding/BindingStore.hs:127-134` —
  `coreBindingStore`
* `packages/opal/src/Opal/Module.hs:253-269` — `newCoreModule`
  / `coreNamespace`

## What the code says

```haskell
coreFormString CoreBegin        = "begin"
coreFormString CoreBeginSyntax  = "begin"          -- same string!
coreFormString CoreDefine       = "define"
coreFormString CoreDefineSyntax = "define-syntax"
...
```

`coreFormSymbol = stringToSymbol . coreFormString` — so both
`CoreBegin` and `CoreBeginSyntax` produce the **same** `Symbol`,
`"begin"`.

Both core forms are then registered in the same maps keyed by that
shared symbol:

```haskell
-- BindingStore.coreBindingStore
coreBindings = [
    let s = coreFormSymbol x
        b = Binding (ScopeSet.singleton def) s
     in (s, b) | x <- [minBound .. maxBound]
  ]

-- Module.newCoreModule
coreNamespace =
  foldr (\form -> over (nsVariable def (coreFormSymbol form))
                       (const (Just (TfmCore form))))
        ns
        coreForms
  where coreForms = [minBound .. maxBound]
```

In both places, whichever of the two forms is inserted *last* wins.
`coreForms = [minBound .. maxBound]` enumerates
`[CoreApp, CoreBegin, CoreBeginSyntax, CoreDefine, ...]`. `foldr f z`
applies `f` right-to-left, so `CoreApp` is the *outermost* (and
therefore last-applied) insert. For the pair we care about,
`CoreBeginSyntax` is inserted *before* `CoreBegin` — so `CoreBegin`
overwrites `CoreBeginSyntax` in the environment, and the entry for
symbol `"begin"` ends up bound to `TfmCore CoreBegin`.

## Effect

Any reference to `begin-for-syntax` in source code becomes an unbound
identifier (no such symbol exists in the binding store), and any
attempt to dispatch via `dispatchCoreForm CoreBeginSyntax` is
unreachable through the normal expansion flow:

* `Expander.expand` looks up an identifier through
  `lookupEnvironment`, which resolves via the binding store; the only
  binding under symbol `"begin"` produces `TfmCore CoreBegin`. The
  `CoreBeginSyntax` branch in `dispatchCoreForm` (currently
  `undefined`) is dead.
* In `partialExpandModuleBegin`, the `TfmCore CoreBeginSyntax -> do
  undefined` branch (`Expander.hs:731-732`) is similarly dead.

The `undefined` placeholders mask the bug: even if `CoreBeginSyntax`
*did* get reached, it would crash immediately. So today the visible
symptom is "begin-for-syntax doesn't work and gives an unbound-error";
once the `undefined`s are filled in, the visible symptom becomes
"begin-for-syntax silently behaves as begin," which is worse.

## What Racket does

Racket's expander treats `begin` and `begin-for-syntax` as distinct
core forms with distinct symbols:

* `begin` is a sequencing form whose body is expanded at the *current*
  phase (`racket/src/expander/expand/main.rkt`, `core-id-form`
  dispatch on `'begin`).
* `begin-for-syntax` raises the expansion phase by one for its body
  (`racket/src/expander/expand/module.rkt`'s `begin-for-syntax`
  branch, which threads through `expand-context-make-next-phase`).

They differ both in the symbol that resolves them and in the dispatch
they perform. They cannot share a name.

## Suggested fix

Two characters:

```haskell
- coreFormString CoreBeginSyntax  = "begin"
+ coreFormString CoreBeginSyntax  = "begin-for-syntax"
```

That fixes the symbol collision in both `coreBindingStore` and
`coreNamespace`. Then implement the two stub branches:

* `Expander.hs:386` — `dispatchCoreForm CoreBeginSyntax stx = undefined`
* `Expander.hs:731` — `TfmCore CoreBeginSyntax -> do undefined`

The latter is the load-bearing one for module bodies (Racket's
`expand-body`'s `'begin-for-syntax` branch is what enables
`define-syntax` to reference compile-time helpers); it needs to call
`nextPhase` around expanding the body, parallel to how `define-syntax`
already does at lines 752-756.

## Aside

`CoreBeginSyntax`'s constructor name and string don't agree with each
other either ("BeginSyntax" vs Racket's "begin-for-syntax"). Renaming
the constructor to `CoreBeginForSyntax` would make the intent
self-documenting and would have made the missing string immediately
obvious.
