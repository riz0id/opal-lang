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
  )
where

import Data.IORef (IORef, readIORef)

import Opal.Writer (Display(..), Doc, (<+>))
import Opal.Writer qualified as Doc
import Opal.Syntax.CoreForm (CoreForm)
import Opal.Syntax (Datum, Identifier)

import Prelude hiding (id)

import System.IO.Unsafe (unsafePerformIO)

-- Transformer -----------------------------------------------------------------

-- | 'Transformer' is the type used to represent compile-time "meanings".
--
-- @since 1.0.0
data Transformer
  = TfmCore CoreForm
    -- ^ 'TfmCore' is a reference to a core form.
  | TfmVar {-# UNPACK #-} !Identifier
    -- ^ 'TfmVar' is a reference to a function argument.
  | TfmVal (IORef Datum)
    -- ^ 'TfmVal' is a compile-time value. In the special case that the
    -- compile-time value is a function, then that function is a macro
    -- transformer.

-- | @since 1.0.0
instance Display Transformer where
  display = \case
    TfmCore core -> docTransformer core
    TfmVar  id   -> docTransformer id
    TfmVal  ref  -> unsafePerformIO (docTransformer <$> readIORef ref)
    where
      docTransformer :: Display a => a -> Doc
      docTransformer x = Doc.string "<transformer: " <+> display x <> Doc.char '>'

-- | @since 1.0.0
instance Show Transformer where
  show = Doc.pretty 80 . display