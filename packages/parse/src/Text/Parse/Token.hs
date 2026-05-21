{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse.Token
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- Tokens used in parse-error messages. A 'Token' is either a single
-- character, a literal string, a special 'TokenEOF' marker for end of
-- input, or a sequence ('Tokens') of nested tokens. The 'Semigroup'
-- instance fuses adjacent character\/string tokens into a single
-- string where possible and falls back to a 'Tokens' sequence for
-- mixed cases.
--
-- @since 1.0.0
module Text.Parse.Token
  ( -- * Token
    Token (..),
    -- ** Basic Operations
    token,
    tokens,
  )
where

import Data.String (IsString (..))

import GHC.Exts (IsList (..))

-- Token -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Token
  = TokenSingle Char
  -- ^ 'TokenSingle' is a token consisting a single character.
  | TokenString String
  -- ^ 'TokenString' is a token consisting of a string of characters.
  | TokenEOF
  -- ^ 'TokenEOF' represents the end of input. Distinct from any
  -- character (including @\'\NUL\'@) so that EOF can be surfaced in
  -- error reports without collapsing into a literal-character match.
  | Tokens [Token]
  -- ^ 'Tokens' is a sequence of tokens.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance IsList Token where
  type Item Token = Token

  toList (Tokens toks) = toks
  toList tok           = [tok]

  fromList = Tokens
  {-# INLINE CONLIKE fromList #-}

-- | 'String' can be converted to a 'Token' via 'tokens'.
--
-- >>> :set -XOverloadedStrings
-- >>> tokens "hello"
-- "hello\NUL"
--
-- @since 1.0.0
instance IsString Token where
  fromString = tokens

-- | @since 1.0.0
instance Semigroup Token where
  TokenString str1 <> TokenString str2 = TokenString (str1 ++ str2)
  TokenSingle chr1 <> TokenString str2 = TokenString (chr1 : str2)
  TokenString str1 <> TokenSingle chr2 = TokenString (str1 ++ [chr2])
  TokenSingle chr1 <> TokenSingle chr2 = TokenString [chr1, chr2]
  Tokens toks1     <> Tokens toks2     = Tokens (toks1 ++ toks2)
  tok1             <> Tokens toks2     = Tokens (tok1 : toks2)
  Tokens toks1     <> tok2             = Tokens (toks1 ++ [tok2])
  -- EOF combines with single chars / strings by becoming a Tokens
  -- list (EOF cannot meaningfully fuse into a string).
  TokenEOF         <> tok2             = Tokens [TokenEOF, tok2]
  tok1             <> TokenEOF         = Tokens [tok1, TokenEOF]

-- | The empty 'Token' is an empty sequence of tokens
--
-- >>> mempty :: Token
-- []
--
-- @since 1.0.0
instance Monoid Token where
  mempty = Tokens []
  {-# INLINE CONLIKE mempty #-}

-- | @since 1.0.0
instance Show Token where
  show (TokenSingle c) = show c
  show (TokenString s) = s
  show TokenEOF        = "<end of input>"
  show (Tokens toks)   = show toks

-- Token - Basic Operations ----------------------------------------------------

-- | Construct a 'Token' from a 'Char'.
--
-- >>> token 'a'
-- 'a'
--
-- @since 1.0.0
token :: Char -> Token
token = TokenSingle
{-# INLINE CONLIKE token #-}

-- | Construct a 'Token' from a 'String'.
--
-- >>> tokens "hello"
-- "hello"
--
-- @since 1.0.0
tokens :: String -> Token
tokens = foldMap token
