
module Opal.Expand.Syntax.BindTable
  ( BindTable,

    -- * Construction
    empty,

    -- * Index
    indexBindingSet,
    index,

    -- * Write
    write,
  )
where

import Data.Data (Data)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Syntax.Binding (Binding)
import Opal.Expand.Syntax.ScopeSet (ScopeSet)

--------------------------------------------------------------------------------

-- | In a scope @sc@, a binding tables is a mapping from a pair 
-- @('Symbol', 'ScopeSet')@ to a 'Binding' in the scope @sc@.
--
-- @since 1.0.0
newtype BindTable :: Type where 
  BindTable :: Map Symbol (Map ScopeSet Binding) -> BindTable
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance Semigroup BindTable where 
  BindTable xs <> BindTable ys = BindTable (Map.unionWith Map.union xs ys)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid BindTable where 
  mempty = BindTable Map.empty
  {-# INLINE mempty #-}

-- Construction ----------------------------------------------------------------

-- | Constructs an empty 'BindTable'.
--
-- @since 1.0.0
empty :: BindTable 
empty = BindTable Map.empty

-- Index -----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
indexBindingSet :: Symbol -> BindTable -> Maybe (Map ScopeSet Binding)
indexBindingSet symbol (BindTable tbl) = Map.lookup symbol tbl

-- | TODO 
--
-- @since 1.0.0
index :: Symbol -> ScopeSet -> BindTable -> Maybe Binding
index symbol set tbl =
  case indexBindingSet symbol tbl of 
    Nothing -> Nothing
    Just rx -> Map.lookup set rx

-- Write -----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
write :: Symbol -> ScopeSet -> Binding -> BindTable -> BindTable 
write symbol set binding (BindTable tbl) = 
  BindTable (Map.alter alter symbol tbl)
  where 
    alter :: Maybe (Map ScopeSet Binding) -> Maybe (Map ScopeSet Binding)
    alter Nothing = Just (Map.singleton set binding)
    alter (Just kvs) = Just (Map.insert set binding kvs)
