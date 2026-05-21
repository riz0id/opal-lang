{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Public combinator surface for @Text.Parse@ — a hand-rolled
-- in-memory parser-combinator library built on pinned-memory
-- buffers ('Text.Parse.Buffer.Buffer'). Distinct from Megaparsec
-- (which @Opal.Reader@ uses): @Text.Parse@ does its own buffer
-- management with raw pointer ops and a custom error model
-- ('Text.Parse.Error.ParseError' carries expected\/received 'Token'
-- sets rather than position-stack).
--
-- The combinator set is the standard parser-combinator zoo:
--
-- * Character primitives — 'consume', 'consume1', 'single',
--   'string', 'satisfy', 'anyChar', 'char', 'oneOf', 'noneOf'.
-- * Character classes — 'digit', 'letter', 'alphaNum', 'space'.
-- * Sequencing — 'many', 'some', 'optional', 'choice', 'between',
--   'sepBy', 'sepBy1', 'endBy', 'manyTill'.
-- * Lookahead\/non-consuming — 'try', 'eof', 'lookAhead',
--   'notFollowedBy'.
--
-- The 'Parse' monad's 'Alternative' instance ('Text.Parse.Monad')
-- provides backtracking '<|>' that restores state on failure; use
-- 'try' to backtrack past arbitrary consumed input.
--
-- The input model is all-in-memory: 'parse' loads the entire input
-- string into a 'Buffer' upfront. Streaming\/chunked input is a
-- deferred design concern — see
-- @review\/issues\/closed\/parse-no-streaming-full-input-required.md@.
--
-- @since 1.0.0
module Text.Parse
  ( -- * Parse
    Parse (..),
    -- ** Basic Operations
    parse,
    runParse,
    -- * Combinators
    -- ** Character primitives
    consume,
    consume1,
    single,
    string,
    satisfy,
    anyChar,
    char,
    oneOf,
    noneOf,
    -- ** Character classes
    digit,
    letter,
    alphaNum,
    space,
    -- ** Repetition / sequencing
    many,
    some,
    optional,
    choice,
    between,
    sepBy,
    sepBy1,
    endBy,
    manyTill,
    -- ** Lookahead / non-consuming
    try,
    eof,
    lookAhead,
    notFollowedBy,
    -- ** Alternative re-exports
    (<|>),
    empty,
  )
where

import Control.Applicative (Alternative (..), optional)
import Control.Monad (when)
import Control.Monad.Except (MonadError (..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.State (MonadState (..))

import Data.Char qualified as Char
import Data.Foldable (traverse_)

import Text.Parse.Error
import Text.Parse.Monad
import Text.Parse.State
import Text.Parse.Token

-- Combinators -----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
consume :: Parse (Maybe Char)
consume = do
  s0 <- get
  liftIO (consumeParseState s0) >>= \case
    Nothing      -> pure Nothing
    Just (c, s1) -> Just c <$ put s1

-- | Consume one character or raise a 'ParseError' at end of input.
-- Unlike 'consume' (which returns 'Nothing' at EOF), 'consume1' is
-- intended for callers that require a character — it reports EOF
-- explicitly via 'TokenEOF' rather than fabricating @\'\NUL\'@.
--
-- @since 1.0.0
consume1 :: Parse Char
consume1 = consume >>= \case
  Nothing -> throwError (newParseError mempty TokenEOF)
  Just c  -> pure c

-- | TODO: docs
--
-- @since 1.0.0
single :: Char -> Parse ()
single c = do
  c' <- consume1
  when (c /= c') do
    throwError (newParseError (token c) (token c'))

-- | TODO: docs
--
-- @since 1.0.0
string :: String -> Parse ()
string = traverse_ single

-- | @'try' p@ runs @p@ and, on failure, restores the parser state to
-- the position before @p@ started. The error is re-thrown so callers
-- can recover via '<|>'. This is the only way to backtrack past
-- arbitrary input — without 'try', '<|>' restores state only when
-- nothing has been consumed.
--
-- @since 1.0.0
try :: Parse a -> Parse a
try parser = do
  s0 <- get
  catchError parser \exn -> do
    put s0
    throwError exn

-- Combinators - Character primitives ------------------------------------------

-- | Consume one character if it satisfies the given predicate.
-- Fails (without consuming) otherwise, and at end of input.
--
-- @since 1.0.0
satisfy :: (Char -> Bool) -> Parse Char
satisfy pred' = do
  s0 <- get
  c  <- consume1
  if pred' c
    then pure c
    else do
      put s0
      throwError (newParseError mempty (token c))

-- | Consume any one character. Fails at end of input. Alias for
-- 'consume1' kept for parity with other parser libraries.
--
-- @since 1.0.0
anyChar :: Parse Char
anyChar = consume1

-- | Like 'single', but returns the consumed character.
--
-- @since 1.0.0
char :: Char -> Parse Char
char c = c <$ single c

-- | Consume any one character from the given list.
--
-- @since 1.0.0
oneOf :: [Char] -> Parse Char
oneOf cs = satisfy (`elem` cs)

-- | Consume any one character /not/ in the given list.
--
-- @since 1.0.0
noneOf :: [Char] -> Parse Char
noneOf cs = satisfy (`notElem` cs)

-- Combinators - Character classes ---------------------------------------------

-- | Consume one ASCII digit @'0'..'9'@.
--
-- @since 1.0.0
digit :: Parse Char
digit = satisfy Char.isDigit

-- | Consume one Unicode letter.
--
-- @since 1.0.0
letter :: Parse Char
letter = satisfy Char.isLetter

-- | Consume one Unicode letter or digit.
--
-- @since 1.0.0
alphaNum :: Parse Char
alphaNum = satisfy Char.isAlphaNum

-- | Consume one Unicode whitespace character.
--
-- @since 1.0.0
space :: Parse Char
space = satisfy Char.isSpace

-- Combinators - Sequencing ----------------------------------------------------

-- | Try each parser in order, returning the first one that succeeds.
-- Equivalent to @'foldr' ('<|>') 'empty'@.
--
-- @since 1.0.0
choice :: [Parse a] -> Parse a
choice = foldr (<|>) empty

-- | @'between' open close p@ runs @open@, then @p@, then @close@,
-- returning @p@\'s result. Common shape for bracketed forms.
--
-- @since 1.0.0
between :: Parse open -> Parse close -> Parse a -> Parse a
between open close p = open *> p <* close

-- | Zero or more occurrences of @p@, separated by @sep@.
--
-- @since 1.0.0
sepBy :: Parse a -> Parse sep -> Parse [a]
sepBy p sep = sepBy1 p sep <|> pure []

-- | One or more occurrences of @p@, separated by @sep@.
--
-- @since 1.0.0
sepBy1 :: Parse a -> Parse sep -> Parse [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | One or more occurrences of @p@, each followed by @sep@.
--
-- @since 1.0.0
endBy :: Parse a -> Parse sep -> Parse [a]
endBy p sep = many (p <* sep)

-- | @'manyTill' p end@ runs @p@ repeatedly until @end@ succeeds.
-- Returns the list of @p@\'s results. @end@ is consumed but its
-- result is discarded.
--
-- @since 1.0.0
manyTill :: Parse a -> Parse end -> Parse [a]
manyTill p end = go
  where
    go = (end *> pure []) <|> ((:) <$> p <*> go)

-- Combinators - Lookahead / non-consuming -------------------------------------

-- | Succeed iff at end of input. Does not consume.
--
-- @since 1.0.0
eof :: Parse ()
eof = do
  s0 <- get
  consume >>= \case
    Nothing -> pure ()
    Just c  -> do
      put s0
      throwError (newParseError TokenEOF (token c))

-- | Run @p@ without consuming input. The parser state is restored
-- regardless of whether @p@ succeeded.
--
-- @since 1.0.0
lookAhead :: Parse a -> Parse a
lookAhead p = do
  s0 <- get
  result <- p
  put s0
  pure result

-- | Succeed iff @p@ /fails/ on the current input. Does not consume.
--
-- @since 1.0.0
notFollowedBy :: Parse a -> Parse ()
notFollowedBy p = do
  s0 <- get
  attempt <- catchError (Right <$> p) (pure . Left)
  put s0
  case attempt of
    Right _ -> throwError mempty
    Left  _ -> pure ()