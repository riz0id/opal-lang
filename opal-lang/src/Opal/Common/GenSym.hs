module Opal.Common.GenSym
  ( -- * TODO
    GenSym (GenSym),
    base,
    uuid,
    symbol,

    -- * TODO
    MonadGenSym,
    newGenSym,
    newGenSymWith,

    -- * TODO
    GenSymM (GenSymM, unGenSymM),
    evalGenSymM,
  )
where

import Data.Data (Data)
import Data.Kind (Type)

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC
import GHC.Records (HasField, getField)

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data GenSym = GenSym (Maybe Symbol) Int
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance HasField "symbol" GenSym Symbol where 
  getField = symbol
  {-# INLINE getField #-}

-- | @since 1.0.0
instance Show GenSym where 
  show = show . symbol
  {-# INLINE CONLIKE show #-}

-- | TODO
--
-- @since 1.0.0
base :: GenSym -> Maybe Symbol
base (GenSym x _) = x
{-# INLINE base #-}

-- | TODO
--
-- @since 1.0.0
uuid :: GenSym -> Int
uuid (GenSym _ x) = x
{-# INLINE uuid #-}

-- | TODO
--
-- @since 1.0.0
symbol :: GenSym -> Symbol
symbol (GenSym Nothing x) = Symbol.pack ("#g:" ++ show x)
symbol (GenSym (Just b) x) = Symbol.pack ("#" ++ shows b ":" ++ show x)
{-# INLINE symbol #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Monad m => MonadGenSym m where 
  newGenSym :: m GenSym

  newGenSymWith :: Symbol -> m GenSym 

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype GenSymM (a :: Type) :: Type where
  GenSymM :: {unGenSymM :: Int# -> (# Int#, a #)} -> GenSymM a

-- | @since 1.0.0
instance Functor GenSymM where
  fmap f (GenSymM k) =
    GenSymM \id0# -> case k id0# of
      (# id1#, x #) -> (# id1#, f x #)
  {-# INLINE fmap #-}

-- | @since 1.0.0
instance Applicative GenSymM where
  pure x = GenSymM \id# -> (# id#, x #)
  {-# INLINE pure #-}

  GenSymM f <*> GenSymM g =
    GenSymM \id0# ->
      let !(# id1#, k #) = f id0#
          !(# id2#, x #) = g id1#
       in (# id2#, k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad GenSymM where
  GenSymM k >>= f =
    GenSymM \id0# -> case k id0# of
      (# id1#, x #) -> unGenSymM (f x) id1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadGenSym GenSymM where 
  newGenSym = GenSymM \id0# -> 
    (# 1# GHC.+# id0#, GenSym Nothing (I# id0#) #)
  {-# INLINE newGenSym #-}

  newGenSymWith idt = GenSymM \id0# -> 
    (# 1# GHC.+# id0#, GenSym (Just idt) (I# id0#) #)
  {-# INLINE newGenSymWith #-}


-- | TODO
--
-- @since 1.0.0
evalGenSymM :: GenSymM a -> a
evalGenSymM (GenSymM k) = case k 0# of (# _, x #) -> x
{-# INLINE evalGenSymM #-}
