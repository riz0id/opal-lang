module Opal.Common.Symbol
  ( -- * TODO
    Symbol (Symbol),

    -- * Construction
    pack,
    unpack,

    -- * Conversion
    toName,

    -- * Query
    size,
    ptr,
  )
where

import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Primitive.Ptr (Ptr)
import Data.String (IsString)
import Data.Text qualified as Text

import GHC.Exts qualified as GHC

import Text.Emit (Emit, emit)
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name

--------------------------------------------------------------------------------

newtype Symbol = Symbol Name
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance IsString Symbol where
  fromString = pack
  {-# INLINE fromString #-}

-- | @since 1.0.0
instance Emit Symbol where
  emit x = Emit.text (Text.pack (unpack x))
  {-# INLINE emit #-}

-- Construction ----------------------------------------------------------------

pack :: String -> Symbol
pack = coerce Name.pack
{-# INLINE pack #-}

unpack :: Symbol -> String
unpack = coerce Name.unpack
{-# INLINE unpack #-}

-- Conversion ------------------------------------------------------------------

toName :: Symbol -> Name
toName = coerce
{-# INLINE toName #-}

-- Query -----------------------------------------------------------------------

size :: Symbol -> Int
size = coerce Name.size
{-# INLINE size #-}

ptr :: Symbol -> Ptr Char
ptr = coerce Name.ptr
{-# INLINE ptr #-}
