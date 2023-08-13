{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.SrcLoc
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- This module defines the 'SrcLoc' type for representing source locations
-- along with basic operations for manipulating source locations.
--
-- @since 1.0.0
module Opal.Common.SrcLoc
  ( -- * SrcLoc
    SrcLoc (..)
    -- ** Basic Operations
  , defaultSrcLoc
  , nextSrcLoc
  , nextLine
  , nextColn
    -- ** Lenses
  , srcLocPosn
  , srcLocLine
  , srcLocColn
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Lens', lens, (.~), (+~), (^.))

import Data.Default (Default (..))
import Data.Function ((&))

import GHC.Generics (Generic)

import Language.Haskell.TH (Pat(..))
import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.TH (Pattern (..))
import Opal.Writer (Display (..))
import qualified Opal.Writer.Doc as Doc

--------------------------------------------------------------------------------

-- | The 'SrcLoc' record is a source file location represented by a position,
-- line, and column number triple.
--
-- @since 1.0.0
data SrcLoc = SrcLoc
  { srcloc_posn :: {-# UNPACK #-} !Int
    -- ^ The position of the 'SrcLoc' in source file. This is the absolute
    -- offset from the beginning of the source file in characters.
  , srcloc_line :: {-# UNPACK #-} !Int
    -- ^ The line number of the 'SrcLoc' in source file.
  , srcloc_coln :: {-# UNPACK #-} !Int
    -- ^ The column number of the 'SrcLoc' in source file.
  }
  deriving (Eq, Generic, Lift, Ord, Show)

-- | 'SrcLoc' defaults to 'defaultSrcLoc'.
--
-- @since 1.0.0
instance Default SrcLoc where
  def = defaultSrcLoc

-- | @since 1.0.0
instance Display SrcLoc where
  display (SrcLoc posn line coln) = display posn <> Doc.char ':' <> display line <> Doc.char ':' <> display coln

-- | @since 1.0.0
instance NFData SrcLoc

-- | @since 1.0.0
instance Pattern SrcLoc where
  liftPat loc =
    (\posn line coln -> ConP 'SrcLoc [] [posn, line, coln])
      <$> liftPat (loc ^. srcLocPosn)
      <*> liftPat (loc ^. srcLocLine)
      <*> liftPat (loc ^. srcLocColn)

-- SrcLoc - Basic Operations ---------------------------------------------------

-- | The default 'SrcLoc'. This source location is located at the beginning of
-- a source file.
--
-- >>> defaultSrcLoc
-- SrcLoc {srcloc_posn = 0, srcloc_line = 1, srcloc_coln = 1}
--
-- @since 1.0.0
defaultSrcLoc :: SrcLoc
defaultSrcLoc = SrcLoc 0 1 1

-- | Increment a source location depending on the given 'Char'. If the case
-- that the given 'Char' is a newline character, then 'nextSrcLoc' will
-- increment the line and position of the given 'SrcLoc'.
--
-- >>> nextSrcLoc '\n' defaultSrcLoc
-- SrcLoc {srcloc_posn = 1, srcloc_line = 2, srcloc_coln = 1}
--
-- Otherwise, 'nextSrcLoc' will increment the column and position of the given
-- 'SrcLoc'.
--
-- >>> nextSrcLoc 'a' defaultSrcLoc
-- SrcLoc {srcloc_posn = 1, srcloc_line = 1, srcloc_coln = 2}
--
-- @since 1.0.0
nextSrcLoc :: Char -> SrcLoc -> SrcLoc
nextSrcLoc c
  | c == '\n' = nextLine
  | otherwise = nextColn

-- | Increment the line and position of a source location.
--
-- >>> nextLine defaultSrcLoc
-- SrcLoc {srcloc_posn = 1, srcloc_line = 2, srcloc_coln = 1}
--
-- @since 1.0.0
nextLine :: SrcLoc -> SrcLoc
nextLine loc =
  loc & srcLocPosn +~ 1
      & srcLocLine +~ 1
      & srcLocColn .~ 1

-- | Increment the column and position of a source location.
--
-- >>> nextColn defaultSrcLoc
-- SrcLoc {srcloc_posn = 1, srcloc_line = 1, srcloc_coln = 2}
--
-- @since 1.0.0
nextColn :: SrcLoc -> SrcLoc
nextColn loc =
  loc & srcLocPosn +~ 1
      & srcLocColn +~ 1

-- SrcLoc - Lenses -------------------------------------------------------------

-- | Lens focusing on the 'srcloc_posn' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocPosn :: Lens' SrcLoc Int
srcLocPosn = lens srcloc_posn \s x -> s { srcloc_posn = x }
{-# INLINE srcLocPosn #-}

-- | Lens focusing on the 'srcloc_line' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocLine :: Lens' SrcLoc Int
srcLocLine = lens srcloc_line \s x -> s { srcloc_line = x }
{-# INLINE srcLocLine #-}

-- | Lens focusing on the 'srcloc_coln' field of 'SrcLoc'.
--
-- @since 1.0.0
srcLocColn :: Lens' SrcLoc Int
srcLocColn = lens srcloc_coln \s x -> s { srcloc_coln = x }
{-# INLINE srcLocColn #-}
