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
-- TODO: docs
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
