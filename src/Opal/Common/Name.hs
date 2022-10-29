{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      :  Opal.Common.Name
-- Copyright   :  (c) Jacob Leach, 2022
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO
--
-- @since 1.0.0
module Opal.Common.Name
  ( Name (Name),

    -- * Conversion
    pack,
    unpack,

    -- * Query
    size,
    null,
    ptr,

    -- * Comparison
    equiv,
  )
where

import Data.Bool.Prim qualified as Bool
import Data.ByteArray.Prim (ByteArray#)
import Data.ByteArray.Prim qualified as ByteArray
import Data.Data (Data, dataTypeOf, gunfold, mkNoRepType, toConstr)
import Data.Ord.Prim ((==#))
import Data.String (IsString)
import Data.Text qualified as Text

import GHC.Exts (Int (I#))
import GHC.Exts qualified as GHC
import GHC.Ptr (Ptr (Ptr))

import Language.Haskell.TH.Syntax (Lift, lift, unsafeCodeCoerce, liftTyped)

import Prelude hiding (null)

import Text.Emit (Emit, emit)
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

-- | 'Name' represents a symbollic name.
--
-- @since 1.0.0
data Name = Name ByteArray#

-- | @since 1.0.0
instance Data Name where
  toConstr _ = error "toConstr on type Name"
  {-# INLINE CONLIKE toConstr #-}

  gunfold _ _ = error "gunfold on type Name"
  {-# INLINE CONLIKE gunfold #-}

  dataTypeOf _ = mkNoRepType "Opal.Common.Name"
  {-# INLINE CONLIKE dataTypeOf #-}

-- | @since 1.0.0
instance Eq Name where
  Name xs# == Name ys# = Bool.toBool (xs# ==# ys#)
  {-# INLINE (==) #-}

-- | @since 1.0.0
instance Lift Name where 
  lift name
    | null name = [| pack "" |]
    | otherwise = [| pack $(lift (unpack name)) |]
  {-# INLINE lift #-}

  liftTyped name = unsafeCodeCoerce (lift name) 
  {-# INLINE liftTyped #-}

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

-- Conversion ------------------------------------------------------------------

-- | \(\mathcal{O}(n)\). Packs the contents of a 'String' into a 'Name'.
--
-- @since 1.0.0
pack :: String -> Name
pack str = Name (ByteArray.pack# (map (fromIntegral . fromEnum) str))
{-# INLINE pack #-}

-- | \(\mathcal{O}(n)\). Unpacks the contents of a 'Name' as a 'String'.
--
-- @since 1.0.0
unpack :: Name -> String
unpack (Name xs#) = map (toEnum . fromIntegral) (ByteArray.unpack# xs#)
{-# INLINE unpack #-}

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtains the length of a 'Name' in characters.
--
-- @since 1.0.0
size :: Name -> Int
size (Name s#) = I# (ByteArray.size# s#)
{-# INLINE size #-}

-- | \(\mathcal{O}(1)\). Obtains the length of a 'Name' in characters.
--
-- @since 1.0.0
null :: Name -> Bool
null nm = size nm == 0 
{-# INLINE null #-}

-- | Obtains a character pointer addressing a 'Name'.
--
-- @since 1.0.0
ptr :: Name -> Ptr Char
ptr (Name s#) = Ptr (ByteArray.address# s#)
{-# INLINE ptr #-}

-- Comparison ------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Tests pointer equivalence of two names.
--
-- @since 1.0.0
equiv :: Name -> Name -> Bool
equiv xs ys = ptr xs == ptr ys 
{-# INLINE equiv #-}
