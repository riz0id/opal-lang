{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Module.Import
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
module Opal.Module.Import
  ( -- * ImportSpec
    ImportSpec (..)
    -- ** Basic Operations
  , importSpecToSyntax
    -- * Import
  , Import (..)
    -- ** Basic Operations
  , defaultImport
  , importToSyntax
    -- ** Optics
  , importPhase
  , importSpecs
  ) where

import Opal.Common.Lens (defineLenses)

import Data.Default (Default (..))

import Opal.Common.Phase (Phase)
import Opal.Common.Symbol (Symbol)
import Opal.Writer (Display (..), (<+>))
import Opal.Writer qualified as Doc

import Prelude hiding (mod)
import Opal.Syntax.TH (syntax)
import Opal.Syntax (Syntax)

-- ImportSpec ------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data ImportSpec
  = ImportSpecPhaseless {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  | ImportSpecForSyntax {-# UNPACK #-} !Symbol
    -- ^ TODO: docs
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display ImportSpec where
  display (ImportSpecPhaseless s) = display s
  display (ImportSpecForSyntax s) = Doc.parens ("for-syntax" <+> Doc.display s)

  displayList = Doc.hsep . map Doc.display

-- | @since 1.0.0
instance Show ImportSpec where
  show = Doc.pretty . Doc.display

-- ImportSpec - Basic Operations -----------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
importSpecToSyntax :: ImportSpec -> Syntax
importSpecToSyntax (ImportSpecPhaseless s) = [syntax| ?s:symbol |]
importSpecToSyntax (ImportSpecForSyntax s) = [syntax| (for-syntax ?s:symbol) |]

-- Import ----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Import = Import
  { import_phase :: {-# UNPACK #-} !Phase
    -- ^ TODO: docs
  , import_specs :: [ImportSpec]
    -- ^ TODO: docs
  }
  deriving (Eq, Ord)

$(defineLenses ''Import)

-- | 'Import' defaults to 'defaultImport'.
--
-- @since 1.0.0
instance Default Import where
  def = defaultImport

-- | @since 1.0.0
instance Display Import where
  display (Import ph spec) =
    (Doc.parens . Doc.hsep)
      [ "import"
      , Doc.parens ("phase" <+> display ph)
      , Doc.indent 2 (displayList spec)
      ]

  displayList = Doc.vsep . map display

-- Import - Basic Operations ---------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
defaultImport :: Import
defaultImport = Import def def

-- | TODO: docs
--
-- @since 1.0.0
importToSyntax :: Import -> Syntax
importToSyntax (Import _ spec) = [syntax| (import ?importSpecStxs ...) |]
  where
    importSpecStxs :: [Syntax]
    importSpecStxs = map importSpecToSyntax spec