{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Opal.Expand.Syntax.BindStore
  ( BindStore (BindStore),

    -- * Construction
    empty,

    -- * Index
    indexBindings,

    -- * Insert
    insert,
  )
where

import Data.Data (Data)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Expand.Syntax.ScopeSet (ScopeSet)
import Opal.Expand.Syntax.Binding (Binder, Binding (scopes, binder))

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype BindStore :: Type where
  BindStore :: Map Name (Map ScopeSet Binder) -> BindStore
  deriving (Data, Eq, Ord, Show)

-- | @since 1.0.0
instance Semigroup BindStore where
  BindStore xs <> BindStore ys = BindStore (Map.unionWith Map.union xs ys)
  {-# INLINE (<>) #-}

-- | @since 1.0.0
instance Monoid BindStore where
  mempty = BindStore Map.empty
  {-# INLINE mempty #-}

-- Construction ----------------------------------------------------------------

-- | Constructs an empty 'BindStore'.
--
-- @since 1.0.0
empty :: BindStore
empty = BindStore Map.empty

-- Index -----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
indexBindings :: Name -> BindStore -> Maybe (Map ScopeSet Binder)
indexBindings name (BindStore kxs) = Map.lookup name kxs

-- Insert ----------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
insert :: Name -> Binding -> BindStore -> BindStore 
insert name binding (BindStore kxs) = 
  BindStore (Map.alter alter name kxs)
  where 
    alter :: Maybe (Map ScopeSet Binder) -> Maybe (Map ScopeSet Binder)
    alter Nothing = Just (Map.singleton binding.scopes binding.binder)
    alter (Just bindings) = Just (Map.insert binding.scopes binding.binder bindings)
