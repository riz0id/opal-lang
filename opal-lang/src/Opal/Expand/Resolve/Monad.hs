{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Opal.Expand.Resolve.Monad
  ( -- * TODO
    ResolveM (R, unR),
  )
where

import Control.Monad.State.Strict (MonadState, get, gets, modify', put, state)

import Data.Foldable (foldr', maximumBy)
import Data.Function (on)
import Data.Kind (Type)

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Opal.Common.GenSym
  ( GenSym (GenSym),
    MonadGenSym,
    newGenSym,
    newGenSymWith,
  )

import Opal.Expand.Resolve.Class (MonadResolve (..))
import Opal.Expand.Syntax.BindTable (BindTable)
import Opal.Expand.Syntax.BindTable qualified as BindTable
import Opal.Expand.Syntax.Binding (Binding, newBindingWith)
import Opal.Expand.Syntax.Binding qualified as Binding
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))
import qualified Opal.Expand.Syntax.MultiScopeSet as MultiScopeSet

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype ResolveM (a :: Type) :: Type where
  R ::
    { unR ::
        BindTable ->
        Int# ->
        Int# ->
        (# BindTable, Int#, Int#, a #)
    } ->
    ResolveM a

-- | @since 1.0.0
instance Functor ResolveM where
  fmap f (R k) =
    R \tbl0 id0# sc0# -> case k tbl0 id0# sc0# of
      (# tbl1, id1#, sc1#, x #) -> (# tbl1, id1#, sc1#, f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative ResolveM where
  pure x = R \tbl id# sc# -> (# tbl, id#, sc#, x #)
  {-# INLINE pure #-}

  R f <*> R g =
    R \tbl0 id0# sc0# ->
      let !(# tbl1, id1#, sc1#, k #) = f tbl0 id0# sc0#
          !(# tbl2, id2#, sc2#, x #) = g tbl1 id1# sc1#
       in (# tbl2, id2#, sc2#, k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad ResolveM where
  R k >>= f =
    R \tbl0 id0# sc0# -> case k tbl0 id0# sc0# of
      (# tbl1, id1#, sc1#, x #) -> unR (f x) tbl1 id1# sc1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadState BindTable ResolveM where
  get = R \tbl id# sc# -> (# tbl, id#, sc#, tbl #)
  {-# INLINE get #-}

  put tbl = R \_ id# sc# -> (# tbl, id#, sc#, () #)
  {-# INLINE put #-}

  state f = R \tbl0 id# sc# -> case f tbl0 of
    (x, tbl1) -> (# tbl1, id#, sc#, x #)
  {-# INLINE state #-}

-- | @since 1.0.0
instance MonadGenSym ResolveM where
  newGenSym = R \tbl id# sc# ->
    (# tbl, 1# GHC.+# id#, sc#, GenSym Nothing (I# id#) #)
  {-# INLINE newGenSym #-}

  newGenSymWith idt = R \tbl id# sc# ->
    (# tbl, 1# GHC.+# id#, sc#, GenSym (Just idt) (I# id#) #)
  {-# INLINE newGenSymWith #-}

-- | @since 1.0.0
instance MonadResolve ResolveM where
  newScopeId = R \tbl id# sc# ->
    (# tbl, id#, 1# GHC.+# sc#, ScopeId (I# sc#) #)
  {-# INLINE newScopeId #-}

  newBind phase idt = do
    let set = MultiScopeSet.index phase idt.multiscope 
    binding <- newBindingWith idt.symbol set
    modify' (BindTable.write idt.symbol set binding)
    pure binding
  {-# INLINE newBind #-}

  resolveBind phase idt = do
    result <- gets (BindTable.indexBindingSet idt.symbol)
    case result of
      Nothing -> do
        binding <- newBind phase idt
        pure (Just binding)
      Just bindings ->
        let binding :: Binding
            binding = maximumBy (compare `on` Binding.scopes) bindings
         in if isAmbiguous binding bindings
              then pure Nothing
              else pure (Just binding)
    where
      isAmbiguous :: Foldable t => Binding -> t Binding -> Bool
      isAmbiguous binding = foldr' ((&&) . Binding.overlaps binding) False
  {-# INLINE resolveBind #-}

  resolve phase idt = do
    result <- resolveBind phase idt
    case result of
      Nothing -> pure Nothing
      Just binding -> pure (Just binding.symbol)
  {-# INLINE resolve #-}
