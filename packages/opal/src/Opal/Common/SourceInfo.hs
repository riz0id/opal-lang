{-# LANGUAGE TemplateHaskell #-}
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

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Lens (defineLenses)
import Opal.Common.SrcLoc (SrcLoc)
import Opal.Writer (Display (..))
import Opal.Writer qualified as Doc

-- SourceInfo ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data SourceInfo = SourceInfo
  { source_info_file_path :: FilePath
    -- ^ The path to the source file that the 'SourceInfo' originated from.
  , source_info_src_loc   :: {-# UNPACK #-} !SrcLoc
    -- ^ The source location that the 'SourceInfo' originated from.
  }
  deriving (Eq, Generic, Lift, Ord, Show)

$(defineLenses ''SourceInfo)

-- | @since 1.0.0
instance Display SourceInfo where
  display (SourceInfo filepath srcloc) = display filepath <> Doc.char ':' <> display srcloc

-- | @since 1.0.0
instance NFData SourceInfo
