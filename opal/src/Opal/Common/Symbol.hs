-- |
-- Module      :  Opal.Common.Symbol
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
module Opal.Common.Symbol
  ( Symbol (Symbol),

    -- * Conversion
    pack,
    unpack,
    fromName,
    toName,

    -- * Query
    size,
    ptr,

    -- * Comparison
    equiv,
  )
where

import Data.Coerce (coerce)
import Data.Data (Data, dataTypeOf, gunfold, mkNoRepType, toConstr)
import Data.Primitive.Ptr (Ptr)
import Data.String (IsString)
import Data.Text qualified as Text

import Text.Emit (Emit, emit)
import Text.Emit qualified as Emit

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name

--------------------------------------------------------------------------------

-- | 'Symbol' represents a quoted 'Name'.
--
-- @since 1.0.0
newtype Symbol = Symbol Name
  deriving (Eq, Ord, IsString)

-- | @since 1.0.0
instance Data Symbol where
  toConstr _ = error "toConstr on type Symbol"
  {-# INLINE CONLIKE toConstr #-}

  gunfold _ _ = error "gunfold on type Symbol"
  {-# INLINE CONLIKE gunfold #-}

  dataTypeOf _ = mkNoRepType "Opal.Common.Symbol"
  {-# INLINE CONLIKE dataTypeOf #-}

-- | @since 1.0.0
instance Emit Symbol where
  emit x = Emit.text (Text.pack (unpack x))
  {-# INLINE emit #-}

-- | @since 1.0.0
instance Show Symbol where
  show x = '\'' : unpack x
  {-# INLINE show #-}

-- Conversion ------------------------------------------------------------------

-- | \(\mathcal{O}(n)\). Packs the contents of a 'String' into a 'Symbol'.
--
-- @since 1.0.0
pack :: String -> Symbol
pack = coerce Name.pack
{-# INLINE pack #-}

-- | \(\mathcal{O}(n)\). Unpacks the contents of a 'Symbol' as a 'String'.
--
-- @since 1.0.0
unpack :: Symbol -> String
unpack = coerce Name.unpack
{-# INLINE unpack #-}

-- | \(\mathcal{O}(1)\). Unpacks the contents of a 'Name' into a 'Symbol'.
--
-- @since 1.0.0
fromName :: Name -> Symbol 
fromName = coerce
{-# INLINE fromName #-}

-- | \(\mathcal{O}(1)\). Unpacks the contents of a 'Symbol' into a 'Name'.
--
-- @since 1.0.0
toName :: Symbol -> Name
toName = coerce
{-# INLINE toName #-}

-- Query -----------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Obtains a character pointer addressing a 'Symbol'.
--
-- @since 1.0.0
size :: Symbol -> Int
size = coerce Name.size
{-# INLINE size #-}

-- | Obtains a character pointer addressing a 'Symbol'.
--
-- @since 1.0.0
ptr :: Symbol -> Ptr Char
ptr = coerce Name.ptr
{-# INLINE ptr #-}

-- Comparison ------------------------------------------------------------------

-- | \(\mathcal{O}(1)\). Tests pointer equivalence of two symbols.
--
-- @since 1.0.0
equiv :: Symbol -> Symbol -> Bool
equiv = coerce Name.equiv
{-# INLINE equiv #-}