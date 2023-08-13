-- |
-- Module      :  Opal.Common.SourceInfo
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
module Opal.Common.SourceInfo
  ( -- * SourceInfo
    SourceInfo (..)
    -- * Optics
  , sourceInfoFilePath
  , sourceInfoSrcLoc
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Lens', lens)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.SrcLoc (SrcLoc)
import Opal.Writer (Display (..))
import Opal.Writer qualified as Doc

-- SourceInfo ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data SourceInfo = SourceInfo
  { source_info_filepath :: FilePath
    -- ^ The path to the source file that the 'SourceInfo' originated from.
  , source_info_srcloc  :: {-# UNPACK #-} !SrcLoc
    -- ^ The source location that the 'SourceInfo' originated from.
  }
  deriving (Eq, Generic, Lift, Ord, Show)

-- | @since 1.0.0
instance Display SourceInfo where
  display (SourceInfo filepath srcloc) = display filepath <> Doc.char ':' <> display srcloc

-- | @since 1.0.0
instance NFData SourceInfo

-- SourceInfo - Optics ---------------------------------------------------------

-- | Lens focusing on the 'source_info_filepath' field of a 'SourceInfo'.
--
-- @since 1.0.0
sourceInfoFilePath :: Lens' SourceInfo FilePath
sourceInfoFilePath = lens source_info_filepath \s x -> s { source_info_filepath = x }

-- | Lens focusing on the 'source_info_srcloc' field of a 'SourceInfo'.
--
-- @since 1.0.0
sourceInfoSrcLoc :: Lens' SourceInfo SrcLoc
sourceInfoSrcLoc = lens source_info_srcloc \s x -> s { source_info_srcloc = x }