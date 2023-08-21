{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.Transformer
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
module Opal.Syntax.Transformer
  ( -- * Transformer
    Transformer (..)
    -- ** Basic Operations
    -- ** Optics
  , transformerCore
  , transformerVal
    -- ** Query
  , isCoreTransformer
  )
where

import Control.Lens (Prism', prism')
import Control.Lens.Extras (is)

import Opal.Core (CoreForm)
import Opal.Writer (Display(..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax (Datum)

import Prelude hiding (id)

-- Transformer -----------------------------------------------------------------

-- | 'Transformer' is the type used to represent compile-time "meanings".
--
-- @since 1.0.0
data Transformer
  = TfmCore  CoreForm
    -- ^ 'TfmCore' is a reference to a core form.
  | TfmDatum Datum
    -- ^ 'TfmVal' is a compile-time value. In the special case that the
    -- compile-time value is a function, then that function is a macro
    -- transformer.
  deriving (Eq, Ord)

-- | @since 1.0.0
instance Display Transformer where
  display = \case
    TfmCore  core -> docTransformer core
    TfmDatum val  -> docTransformer val
    where
      docTransformer :: Display a => a -> Doc
      docTransformer x = Doc.string "<transformer:" <+> display x <> Doc.char '>'

-- | @since 1.0.0
instance Show Transformer where
  show = Doc.pretty . display

-- Transformer - Basic Operations ----------------------------------------------

-- Transformer - Optics --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
transformerCore :: Prism' Transformer CoreForm
transformerCore = prism' TfmCore \case TfmCore x -> Just x; _ -> Nothing

-- | TODO: docs
--
-- @since 1.0.0
transformerVal :: Prism' Transformer Datum
transformerVal = prism' TfmDatum \case TfmDatum x -> Just x; _ -> Nothing

-- Transformer - Query ---------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
isCoreTransformer :: Transformer -> Bool
isCoreTransformer = is transformerCore