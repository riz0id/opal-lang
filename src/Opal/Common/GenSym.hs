module Opal.Common.GenSym
  ( -- * TODO
    GenSym (GenSym, base, uuid),
    makeGenSym,

    -- * Conversion
    toName,
    toSymbol,

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
import Data.Maybe (fromMaybe)

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC

--------------------------------------------------------------------------------

import Opal.Common.Name (Name)
import Opal.Common.Name qualified as Name
import Opal.Common.Symbol (Symbol)
import Opal.Common.Symbol qualified as Symbol

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data GenSym = GenSym
  { base :: {-# UNPACK #-} !Symbol
  , uuid :: {-# UNPACK #-} !Int
  }
  deriving (Data, Eq, Ord)

-- | @since 1.0.0
instance Show GenSym where
  show = show . toSymbol
  {-# INLINE CONLIKE show #-}

-- | TODO
--
-- @since 1.0.0
makeGenSym :: Maybe Symbol -> Int -> GenSym
makeGenSym name = GenSym (fromMaybe defaultSymbol name)

-- | TODO
--
-- @since 1.0.0
defaultSymbol :: Symbol
defaultSymbol = Symbol.pack "g"

-- GenSym - Conversion ---------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
toName :: GenSym -> Name
toName gensym = Name.pack (show gensym.base ++ show gensym.uuid)

-- | TODO
--
-- @since 1.0.0
toSymbol :: GenSym -> Symbol
toSymbol gensym = Symbol.pack (show gensym.base ++ show gensym.uuid)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
class Monad m => MonadGenSym m where
  newGenSym :: m GenSym
  newGenSym = newGenSymWith defaultSymbol
  {-# INLINE newGenSym #-}

  newGenSymWith :: Symbol -> m GenSym

  {-# MINIMAL newGenSymWith #-}

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
    (# 1# GHC.+# id0#, GenSym defaultSymbol (I# id0#) #)
  {-# INLINE newGenSym #-}

  newGenSymWith idt = GenSymM \id0# ->
    (# 1# GHC.+# id0#, GenSym idt (I# id0#) #)
  {-# INLINE newGenSymWith #-}

-- | TODO
--
-- @since 1.0.0
evalGenSymM :: GenSymM a -> a
evalGenSymM (GenSymM k) = case k 0# of (# _, x #) -> x
{-# INLINE evalGenSymM #-}
