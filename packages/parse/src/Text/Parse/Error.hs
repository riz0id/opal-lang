{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse.Error
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
module Text.Parse.Error
  ( -- * ParseError
    ParseError (..),
    -- ** Basic Operations
    newParseError,
    -- ** Lenses
    parseErrorExpected,
    parseErrorRecieved,
  )
where

import Control.Lens (Lens', lens)

import Data.Set (Set)
import Data.Set qualified as Set

import Text.Parse.Token

-- ParseError ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ParseError = ParseError
  { parse_error_expected :: Set Token
  -- ^ The tokens that were expected to be recieved at the point of the
  -- 'ParseError'.
  , parse_error_recieved :: Set Token
  -- ^ The tokens that were actually recieved at the point of the 'ParseError'.
  }
  deriving (Eq, Ord, Show)

-- | @since 1.0.0
instance Monoid ParseError where
  mempty = ParseError Set.empty Set.empty

-- | @since 1.0.0
instance Semigroup ParseError where
  ParseError e1 r1 <> ParseError e2 r2 = ParseError (e1 <> e2) (r1 <> r2)

-- ParseError - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newParseError ::
  -- | The expected 'Token'.
  Token ->
  -- | The recieved 'Token'.
  Token ->
  -- | TODO: docs
  ParseError
newParseError expected recieved =
  ParseError
    { parse_error_expected = Set.singleton expected
    , parse_error_recieved = Set.singleton recieved
    }

-- ParseError - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'parse_error_expected' field of 'ParseError'.
--
-- @since 1.0.0
parseErrorExpected :: Lens' ParseError (Set Token)
parseErrorExpected = lens parse_error_expected \s x -> s { parse_error_expected = x }

-- | Lens focusing on the 'parse_error_recieved' field of 'ParseError'.
--
-- @since 1.0.0
parseErrorRecieved :: Lens' ParseError (Set Token)
parseErrorRecieved = lens parse_error_recieved \s x -> s { parse_error_recieved = x }

