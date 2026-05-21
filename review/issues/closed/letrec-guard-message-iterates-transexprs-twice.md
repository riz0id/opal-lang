# `expandLetRec`'s bad-syntax guard message iterates `transExprs` twice instead of `valExprs`

**Severity:** low (affects only the error message produced when
`guardExpressionContext` rejects the form; the expander otherwise
proceeds correctly. But the constructed shape is observably wrong and
can mislead a user trying to read the error.)

**Location:** `packages/opal/src/Opal/Expander.hs:586-589`

## What the code says

```haskell
expandLetRec transExprs valExprs expr = do

  guardExpressionContext
    let stxs = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
     in [syntax| (letrec-syntaxes+values (?stxs ...) (?vals ...) ?expr) |]
```

Both `stxs` and `vals` are constructed from `transExprs`. The
`valExprs` argument is unused in the guard's reconstructed shape.

The intent is clearly:

```haskell
    let stxs = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) valExprs  -- not transExprs
```

i.e. one binding-pair list per source-form group: syntax bindings
from `transExprs`, value bindings from `valExprs`. With the bug, the
error reconstruction shows the syntax-binding list as both groups,
which is misleading the moment they differ (which is the usual case).

## Why this is just a guard message

`guardExpressionContext` reads the current expansion context from the
`Reader` and only throws if the context is wrong. The `[syntax| … |]`
expression is built solely to be passed to the error constructor; the
actual expansion proceeds against the original `transExprs`/`valExprs`
arguments downstream. So the bug doesn't change which bindings get
expanded — it only changes what the user sees in the error report
when the form is in the wrong context.

## Suggested fix

One word, line 588:

```haskell
-        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) transExprs
+        vals = map (\(id, stx) -> [syntax| (?id:id ?stx) |]) valExprs
```
