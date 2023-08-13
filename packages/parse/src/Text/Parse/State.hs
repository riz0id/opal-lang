{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Text.Parse.State
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
module Text.Parse.State
  ( -- * ParseState
    ParseState (..),
    -- ** Basic Operations
    newParseState,
    consumeParseState,
    -- ** Lenses
    parseBuffer,
    parseOffset,
  )
where

import Control.Lens (Lens', lens, over, (^.))

import Text.Parse.Buffer (Buffer, sizeofBuffer, readUtf8OffBuffer)

-- ParseState ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ParseState = ParseState
  { parse_buffer :: Buffer
    -- ^ TODO: docs
  , parse_offset :: Int
    -- ^ TODO: docs
  }
  deriving Show

-- ParseState - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
newParseState :: Buffer -> ParseState
newParseState buffer =
  ParseState
    { parse_buffer = buffer
    , parse_offset = 0
    }

-- | TODO: docs
--
-- @since 1.0.0
consumeParseState :: ParseState -> IO (Maybe (Char, ParseState))
consumeParseState s0 = do
  let buf = s0 ^. parseBuffer
      len = sizeofBuffer buf
      off = s0 ^. parseOffset
   in if off < len
        then do
          (c, n) <- readUtf8OffBuffer buf off
          pure (Just (c, over parseOffset (n +) s0))
        else
          pure Nothing

-- ParseState - Lenses ---------------------------------------------------------

-- | Lens focusing on the 'parse_buffer' field of 'ParseState'.
--
-- @since 1.0.0
parseBuffer :: Lens' ParseState Buffer
parseBuffer = lens parse_buffer \s x -> s { parse_buffer = x }

-- | Lens focusing on the 'parse_buffer' field of 'ParseState'.
--
-- @since 1.0.0
parseOffset :: Lens' ParseState Int
parseOffset = lens parse_offset \s x -> s { parse_offset = x }