module Opal.Common.Name
  ( -- * TODO
    Name (Name),

    -- * Construction
    pack,
    unpack,

    -- * Query
    size,
    ptr,
  )
where

import Data.Bool.Prim qualified as Bool
import Data.ByteArray.Prim (ByteArray#)
import Data.ByteArray.Prim qualified as ByteArray
import Data.Data (Data, dataTypeOf, gunfold, mkNoRepType, toConstr)
import Data.Kind (Type)
import Data.Ord.Prim ((==#))
import Data.String (IsString)
import Data.Text qualified as Text

import GHC.Exts (Int (I#))
import GHC.Exts qualified as GHC
import GHC.Ptr (Ptr (Ptr))

import Text.Emit (Emit, emit)
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

data Name :: Type where
  Name :: ByteArray# -> Name

-- | @since 1.0.0
instance Data Name where
  toConstr _ = error "toConstr on type Name"
  {-# INLINE CONLIKE toConstr #-}

  gunfold _ _ = error "gunfold on type Name"
  {-# INLINE CONLIKE gunfold #-}

  dataTypeOf _ = mkNoRepType "Opal.Name"
  {-# INLINE CONLIKE dataTypeOf #-}

-- | @since 1.0.0
instance Eq Name where
  Name xs# == Name ys# = Bool.toBool (xs# ==# ys#)
  {-# INLINE (==) #-}

-- | @since 1.0.0
instance Ord Name where
  compare xs ys = compare (unpack xs) (unpack ys)
  {-# INLINE compare #-}

-- | @since 1.0.0
instance Emit Name where
  emit x = Emit.text (Text.pack (unpack x))
  {-# INLINE emit #-}

-- | @since 1.0.0
instance Show Name where
  show = unpack
  {-# INLINE show #-}

-- | @since 1.0.0
instance IsString Name where
  fromString = pack
  {-# INLINE fromString #-}

-- Construction ----------------------------------------------------------------

pack :: String -> Name
pack str = Name (ByteArray.pack# (map (fromIntegral . fromEnum) str))
{-# INLINE pack #-}

unpack :: Name -> String
unpack (Name xs#) = map (toEnum . fromIntegral) (ByteArray.unpack# xs#)
{-# INLINE unpack #-}

-- Query -----------------------------------------------------------------------

size :: Name -> Int
size (Name s#) = I# (ByteArray.size# s#)
{-# INLINE size #-}

ptr :: Name -> Ptr Char
ptr (Name s#) = Ptr (ByteArray.address# s#)
{-# INLINE ptr #-}
