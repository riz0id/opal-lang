{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Expand.Syntax.BindStore
  ( BindStore (BindStore),

    -- * Construction
    empty,
    coreSyntax,

    -- * Index
    index,

    -- * Insert
    insert,
    insertCoreForm,
  )
where

import Data.Data (Data)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import GHC.Exts (IsList, Item, fromList, toList)

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)

import Opal.Core.CoreForm (CoreForm)
import Opal.Core.CoreForm qualified as CoreForm
import Opal.Expand.Syntax.Binding (Binder, Binding)
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.ScopeSet (ScopeSet)

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

-- | @since 1.0.0
instance IsList BindStore where
  type Item BindStore = (Name, Binding)

  fromList = foldr (uncurry insert) empty
  {-# INLINE toList #-}
  toList BindStore {} = undefined
  {-# INLINE fromList #-}

-- Construction ----------------------------------------------------------------

-- | Constructs an empty 'BindStore'.
--
-- @since 1.0.0
empty :: BindStore
empty = BindStore Map.empty

-- | TODO
--
-- @since 1.0.0
coreSyntax :: BindStore
coreSyntax =
  let forms :: [CoreForm]
      forms =
        [ CoreForm.CoreFormLambda
        , CoreForm.CoreFormLetSyntax
        , CoreForm.CoreFormSyntax
        ]
   in foldr insertCoreForm empty forms

-- Index -----------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
index :: Name -> BindStore -> Set Binding
index name (BindStore kxs) =
  case Map.lookup name kxs of
    Nothing -> Set.empty
    Just rx -> Map.foldrWithKey' (\sc -> Set.insert . Binding.Binding sc) Set.empty rx

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

-- | TODO
--
-- @since 1.0.0
insertCoreForm :: CoreForm -> BindStore -> BindStore
insertCoreForm prim = insert (CoreForm.primToName prim) (Binding.makePrimBinding prim)