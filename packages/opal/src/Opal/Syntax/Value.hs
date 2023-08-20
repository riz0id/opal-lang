{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.Value
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
module Opal.Syntax.Value
  ( -- * Value
    Value (..)
    -- ** Optics
  , valueBool
  , valueChar
  , valueSymbol
  , valueF32
  , valueI32
  )
where

import Control.DeepSeq (NFData)

import Control.Lens (Prism', prism')

import Data.Int (Int32)

import GHC.Generics (Generic)

import Language.Haskell.TH.Syntax (Lift)

import Opal.Common.Symbol (Symbol)
import Opal.Writer (Display(..))
import Opal.Writer qualified as Doc

import Prelude hiding (id)

-- Value -----------------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
data Value
  = ValueB   !Bool
    -- ^ A literal boolean value.
  | ValueC   {-# UNPACK #-} !Char
    -- ^ A literal character value.
  | ValueS   {-# UNPACK #-} !Symbol
    -- ^ A literal symbol value.
  | ValueF32 {-# UNPACK #-} !Float
    -- ^ A literal 32-bit floating point number value.
  | ValueI32 {-# UNPACK #-} !Int32
    -- ^ A literal 32-bit integer value.
  deriving (Eq, Generic, Ord, Lift)

-- | @since 1.0.0
instance Display Value where
  display (ValueB    x) = if x then Doc.string "#t" else Doc.string "#f"
  display (ValueC    x) = Doc.string "#\\" <> Doc.char x
  display (ValueS    x) = display x
  display (ValueF32  x) = Doc.string (show x)
  display (ValueI32  x) = Doc.string (show x)

-- | @since 1.0.0
instance NFData Value

-- | @since 1.0.0
instance Show Value where
  show = Doc.pretty . display

-- Value - Optics --------------------------------------------------------------

-- | Prism focusing on the 'ValueB' constructor of 'Datum'.
--
-- @since 1.0.0
valueBool :: Prism' Value Bool
valueBool = prism' ValueB \case { ValueB x -> Just x; _ -> Nothing }
{-# INLINE valueBool #-}

-- | Prism focusing on the 'ValueC' constructor of 'Datum'.
--
-- @since 1.0.0
valueChar :: Prism' Value Char
valueChar = prism' ValueC \case { ValueC x -> Just x; _ -> Nothing }
{-# INLINE valueChar #-}

-- | Prism focusing on the 'ValueS' constructor of 'Datum'.
--
-- @since 1.0.0
valueSymbol :: Prism' Value Symbol
valueSymbol = prism' ValueS \case { ValueS x -> Just x; _ -> Nothing }
{-# INLINE valueSymbol #-}

-- | Prism focusing on the 'ValueF32' constructor of 'Datum'.
--
-- @since 1.0.0
valueF32 :: Prism' Value Float
valueF32 = prism' ValueF32 \case { ValueF32 x -> Just x; _ -> Nothing }
{-# INLINE valueF32 #-}

-- | Prism focusing on the 'DatumI32' constructor of 'Datum'.
--
-- @since 1.0.0
valueI32 :: Prism' Value Int32
valueI32 = prism' ValueI32 \case { ValueI32 x -> Just x; _ -> Nothing }
{-# INLINE valueI32 #-}