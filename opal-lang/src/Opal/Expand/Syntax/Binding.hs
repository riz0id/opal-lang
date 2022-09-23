{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Expand.Syntax.Binding
  ( -- * TODO
    Binding (GenBinding, SymBinding),
    scopes,
    symbol,

    -- * Construction
    newBinding,
    newBindingWith,

    -- * Scope Set Operations
    overlaps,
    superset,
  )
where

import Data.Data (Data)
import Data.Kind (Type)

import GHC.Records (HasField, getField)

--------------------------------------------------------------------------------

import Opal.Common.GenSym (GenSym, MonadGenSym)
import Opal.Common.GenSym qualified as GenSym
import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.ScopeSet qualified as ScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Binding :: Type where
  GenBinding :: ScopeSet -> GenSym -> Binding
  SymBinding :: ScopeSet -> Symbol -> Binding
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance HasField "symbol" Binding Symbol where
  getField (GenBinding _ gen) = gen.symbol
  getField (SymBinding _ sym) = sym
  {-# INLINE getField #-}

-- | @since 1.0.0
instance HasField "scopes" Binding ScopeSet where
  getField (GenBinding scps _) = scps
  getField (SymBinding scps _) = scps
  {-# INLINE getField #-}

-- | TODO
--
-- @since 1.0.0
symbol :: Binding -> Symbol
symbol (GenBinding _ gen) = GenSym.symbol gen
symbol (SymBinding _ sym) = sym
{-# INLINE symbol #-}

-- | TODO
--
-- @since 1.0.0
scopes :: Binding -> ScopeSet
scopes (GenBinding scps _) = scps
scopes (SymBinding scps _) = scps
{-# INLINE scopes #-}

-- Construction ----------------------------------------------------------------

-- | Constructs a new 'Binding' that binds a generated symbol to the set of
-- scopes.
--
-- @since 1.0.0
newBinding :: MonadGenSym m => ScopeSet -> m Binding
newBinding scps = fmap (GenBinding scps) GenSym.newGenSym

-- | Like 'newBinding', but uses the provided 'Symbol' as the base 'Symbol' for
-- the binding's generated symbol.
--
-- @since 1.0.0
newBindingWith :: MonadGenSym m => Symbol -> ScopeSet -> m Binding
newBindingWith s scps = fmap (GenBinding scps) (GenSym.newGenSymWith s)

-- Scope Set Operations --------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
overlaps :: Binding -> Binding -> Bool
overlaps a b = ScopeSet.overlaps (scopes a) (scopes b)

-- | TODO
--
-- @since 1.0.0
superset :: Binding -> Binding -> Bool
superset a b = ScopeSet.superset (scopes a) (scopes b)