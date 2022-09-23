
module Opal.Common.Symbol
  ( -- * TODO
    Symbol (Symbol),

    -- * Construction
    pack,
    unpack,
    -- sliceByteArray,

    -- * Query
    size,
    ptr,
  )
where

import Data.Data (Data, dataTypeOf, gunfold, mkNoRepType, toConstr)
import Data.Kind (Type)
import Data.String (IsString)
import Data.Bool.Prim qualified as Bool
import Data.CharArray.Prim (CharArray#)
import Data.CharArray.Prim qualified as CharArray

import GHC.Exts (Int (I#))
import GHC.Exts qualified as GHC
import GHC.Ptr (Ptr (Ptr))

import Prettyprinter (Pretty, (<+>))
import Prettyprinter qualified as Print

--------------------------------------------------------------------------------

data Symbol :: Type where
  Symbol :: CharArray# -> Symbol

instance Data Symbol where
  toConstr _ = error "toConstr on type Symbol"
  {-# INLINE CONLIKE toConstr #-}

  gunfold _ _ = error "gunfold on type Symbol"
  {-# INLINE CONLIKE gunfold #-}

  dataTypeOf _ = mkNoRepType "Opal.Symbol"
  {-# INLINE CONLIKE dataTypeOf #-}

instance Eq Symbol where
  Symbol xs# == Symbol ys# = Bool.toBool (CharArray.eq# xs# ys#)
  {-# INLINE (==) #-}

instance Ord Symbol where
  compare xs ys = compare (unpack xs) (unpack ys)
  {-# INLINE compare #-}

-- | @since 1.0.0
instance Pretty Symbol where
  pretty x = Print.squote <> Print.viaShow x
  {-# INLINE pretty #-}

  prettyList xs = Print.parens (foldr ((<+>) . Print.viaShow) Print.emptyDoc xs)
  {-# INLINE prettyList #-}

instance Show Symbol where
  show = unpack
  {-# INLINE show #-}

instance IsString Symbol where
  fromString = pack 
  {-# INLINE fromString #-}

-- Construction ----------------------------------------------------------------

pack :: String -> Symbol
pack str = Symbol (CharArray.pack# str)
{-# INLINE pack #-}

unpack :: Symbol -> String
unpack (Symbol xs#) = CharArray.unpack# xs#
{-# INLINE unpack #-}

-- Query -----------------------------------------------------------------------

size :: Symbol -> Int
size (Symbol s#) = I# (CharArray.size# s#)
{-# INLINE size #-}

ptr :: Symbol -> Ptr Char
ptr (Symbol s#) = Ptr (CharArray.address# s#)
{-# INLINE ptr #-}
