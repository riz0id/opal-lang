# `Text.Parse.consume1` silently returns `'\NUL'` at end of input

**Severity:** high (collapses two semantically distinct conditions —
"got NUL char" and "end of input" — into one value, so combinators
built on `consume1` cannot tell them apart)

**Location:** `packages/parse/src/Text/Parse.hs:57-58`

## What the code says

```haskell
consume :: Parse (Maybe Char)
consume = do
  s0 <- get
  liftIO (consumeParseState s0) >>= \case
    Nothing      -> pure Nothing
    Just (c, s1) -> Just c <$ put s1

consume1 :: Parse Char
consume1 = fmap (fromMaybe '\NUL') consume
```

`consume` correctly returns `Maybe Char` — `Nothing` for EOF, `Just c`
for a successfully-consumed character. `consume1` then collapses the
`Nothing` (EOF) into `'\NUL'`.

The intent appears to be "give me a single char, EOF is irrelevant" —
but the EOF condition needs to surface somehow. Returning `'\NUL'`
silently is wrong on two counts:

1. **NUL is a valid character.** A parser walking through a UTF-8
   file containing a literal NUL byte (rare but legitimate) would
   appear to hit EOF, breaking position tracking.

2. **EOF is a parse error.** A caller of `consume1` expects to have
   actually consumed a character. If the input is exhausted, the
   caller's logic (e.g. "the next char must be `'('`") may
   accidentally succeed or fail in surprising ways depending on what
   it compares against. With the current implementation:

   ```haskell
   single c = do
     c' <- consume1
     when (c /= c') do
       throwError (newParseError (token c) (token c'))
   ```

   At EOF, `c' = '\NUL'`. If `c == '\NUL'` (the caller wanted a NUL),
   `single` succeeds *without consuming anything*. If `c /= '\NUL'`,
   `single` reports `expected c, received NUL` — misleading the
   user about what happened.

## What it should do

`consume1` should throw a `ParseError` at EOF, with the `received`
field denoting "end of input" rather than a fake `'\NUL'` token.

The `Token` ADT (`Text.Parse.Token`) doesn't currently have an EOF
constructor — only `TokenSingle Char`, `TokenString String`, and
`Tokens [Token]`. Adding `TokenEOF` (or `TokenEnd`) would let
`consume1` report the actual condition:

```haskell
consume1 :: Parse Char
consume1 = do
  consume >>= \case
    Nothing -> throwError (newParseError mempty tokenEOF)
    Just c  -> pure c
```

The `expected` argument is `mempty` because the *caller* of
`consume1` (e.g. `single`) knows what it expected and can `catchError`
to substitute its own expected token, or use `<?>`-style labels (not
yet implemented).

## Suggested fix

Two pieces:

1. Add a `TokenEOF` constructor to `Token`.
2. Rewrite `consume1` to throw rather than fabricate:

```haskell
consume1 :: Parse Char
consume1 = consume >>= \case
  Nothing -> throwError (newParseError mempty TokenEOF)
  Just c  -> pure c
```

Optional follow-up: make `single`'s error message use the caller's
`expected` token combined with whatever `consume1` raised, so the
final message says `expected '(', received <end of input>` instead of
`expected '(', received NUL`.

## Cross-reference

This bug compounds with
[[parse-combinator-library-incomplete]]: a parser library without
EOF awareness can't implement combinators like `eof`, `notFollowedBy`,
or `lookAhead` correctly. Adding EOF tokens is a prerequisite for
those.
