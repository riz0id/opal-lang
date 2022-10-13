{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Expand.Common.Intern
  ( -- * TODO
    InternIO,
    runInternIO,

    -- * TODO
    Intern (I, unI),
    runInternST,

    -- * TODO
    internData,
    intern,
  )
where

import Control.Monad.State.Strict (state)

import Data.Data (Data, gmapM)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Set.Internal qualified as Set

import GHC.Exts qualified as GHC
import GHC.IO (IO (IO))

import Type.Reflection
  ( TypeRep,
    eqTypeRep,
    typeOf,
    typeRep,
    type (:~~:) (HRefl),
  )

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

import Opal.Expand.Common.Intern.Monad (Intern (I, unI), InternIO)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runInternIO :: InternIO a -> IO a
runInternIO (I k) =
  IO \st0# ->
    let !(# st1#, mut# #) = GHC.newMutVar# Set.empty st0#
        !(# st2#, result #) = k mut# st1#
     in (# st2#, result #)
{-# INLINE runInternIO #-}

-- | TODO
--
-- @since 1.0.0
runInternST :: (forall s. Intern s a) -> a
runInternST (I k) =
  GHC.runRW# \st0# ->
    let !(# st1#, mut# #) = GHC.newMutVar# Set.empty st0#
        !(# _, result #) = k mut# st1#
     in result
{-# INLINE runInternST #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
internData :: Data a => a -> a
internData dat = runInternST (internDataM dat)
  where
    internDataM :: Data a => a -> Intern s a
    internDataM = gmapM \subterm ->
      case eqTypeRep (typeOf subterm) symTypeRep of
        Nothing -> internDataM subterm
        Just HRefl -> intern subterm

    symTypeRep :: TypeRep Symbol
    symTypeRep = typeRep
{-# INLINE internData #-}

-- | TODO
--
-- @since 1.0.0
intern :: Symbol -> Intern s Symbol
intern ref =
  state \set -> case find 0 ref set of
    Nothing -> (ref, Set.insert ref set)
    Just sym -> (sym, set)
  where
    find :: Int -> Symbol -> Set Symbol -> Maybe Symbol
    find !_ !_ Set.Tip = Nothing
    find idx x (Set.Bin _ kx l r) = case compare x kx of
      LT -> find idx x l
      GT -> find (idx + Set.size l + 1) x r
      EQ -> Just $! kx
{-# INLINE intern #-}