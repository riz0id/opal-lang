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

import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name

--------------------------------------------------------------------------------

newtype Symbol = Symbol Name
  deriving (Data, Eq, Ord, Show)

instance IsString Symbol where
  fromString = pack
  {-# INLINE fromString #-}

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
