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
-- Parse error type. A 'ParseError' carries two 'Set's of 'Token's:
-- the tokens the parser /expected/ and the tokens it /received/.
-- The 'Semigroup' instance unions both sets, so alternatives
-- accumulate expected-token sets when multiple branches fail — used
-- by 'Text.Parse.Monad.Alternative' \'s '<|>' to build a useful
-- diagnostic without losing information from earlier branches.
--
-- @since 1.0.0
module Text.Parse.Error
  ( -- * ParseError
    ParseError (..),
    -- ** Basic Operations
    newParseError,
    -- ** Lenses
    parseErrorExpected,
    parseErrorReceived,
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
  -- ^ The tokens that were expected to be received at the point of the
  -- 'ParseError'.
  , parse_error_received :: Set Token
  -- ^ The tokens that were actually received at the point of the 'ParseError'.
  }
  deriving (Eq, Show)
  -- Ord intentionally not derived: the natural lexicographic order
  -- (by expected then received Token sets) bears no relationship to
  -- source position or severity. Code that wants to sort errors
  -- should choose its criterion explicitly. See
  -- review/issues/closed/parse-error-ord-instance-may-not-be-meaningful.md.

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
  -- | The received 'Token'.
  Token ->
  -- | TODO: docs
  ParseError
newParseError expected received =
  ParseError
    { parse_error_expected = Set.singleton expected
    , parse_error_received = Set.singleton received
    }

-- ParseError - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'parse_error_expected' field of 'ParseError'.
--
-- @since 1.0.0
parseErrorExpected :: Lens' ParseError (Set Token)
parseErrorExpected = lens parse_error_expected \s x -> s { parse_error_expected = x }

-- | Lens focusing on the 'parse_error_received' field of 'ParseError'.
--
-- @since 1.0.0
parseErrorReceived :: Lens' ParseError (Set Token)
parseErrorReceived = lens parse_error_received \s x -> s { parse_error_received = x }

