{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Module.Export
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
module Opal.Module.Export
  ( -- * ExportSpec
    ExportSpec (..)
    -- ** Basic Operations
  , exportSpecToSyntax
    -- * Export
  , Export (..)
    -- ** Basic Operations
  , defaultExport
  , exportPhaseLevels
  , exportToSyntax
    -- ** Optics
  , exportPhase
  , exportSpecs
  ) where

import Opal.Common.Lens (defineLenses)

import Data.Default (Default (..))

import Opal.Common.Phase (PhaseShift)
import Opal.Syntax (Identifier, Syntax)
import Opal.Syntax.TH (syntax)
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (mod)

-- ExportSpec ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ExportSpec
  = ExportSpecPhaseless {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  | ExportSpecForSyntax {-# UNPACK #-} !Identifier
    -- ^ TODO: docs
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display ExportSpec where
  display (ExportSpecPhaseless s) = display s
  display (ExportSpecForSyntax s) = Doc.parens ("for-syntax" <+> Doc.display s)

  displayList = Doc.hsep . map Doc.display

-- | @since 1.0.0
instance Show ExportSpec where
  show = Doc.pretty . Doc.display

-- ExportSpec - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
exportSpecToSyntax :: ExportSpec -> Syntax
exportSpecToSyntax (ExportSpecPhaseless s) = [syntax| ?s:id |]
exportSpecToSyntax (ExportSpecForSyntax s) = [syntax| (for-syntax ?s:id) |]

-- Export ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Export = Export
  { export_phase :: {-# UNPACK #-} !PhaseShift
    -- ^ TODO: docs
  , export_specs :: [ExportSpec]
    -- ^ TODO: docs
  }
  deriving (Eq, Ord)

$(defineLenses ''Export)

-- | 'Export' defaults to 'defaultExport'.
--
-- @since 1.0.0
instance Default Export where
  def = defaultExport

-- | @since 1.0.0
instance Display Export where
  display (Export _ spec) =
    (Doc.parens . Doc.hsep)
      [ "export"
      , Doc.indent 2 (displayList spec)
      ]

  displayList = Doc.vsep . map display

-- Export - Basic Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defaultExport :: Export
defaultExport = Export def def

-- | TODO: docs
--
-- @since 1.0.0
exportPhaseLevels :: Export -> [(PhaseShift, Identifier)]
exportPhaseLevels (Export sh spec) = foldr run [] spec
  where
    run :: ExportSpec -> [(PhaseShift, Identifier)] -> [(PhaseShift, Identifier)]
    run (ExportSpecPhaseless s) xs = (sh, s) : xs
    run (ExportSpecForSyntax s) xs = (1 + sh, s) : xs


-- | TODO: docs
--
-- @since 1.0.0
exportToSyntax :: Export -> Syntax
exportToSyntax (Export _ spec) = [syntax| (export ?exportSpecStxs ...) |]
  where
    exportSpecStxs :: [Syntax]
    exportSpecStxs = map exportSpecToSyntax spec