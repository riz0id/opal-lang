{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Opal.Repl.Input.Monad
  ( -- * TODO
    runInputIO,
    
    -- * TODO
    seek,
    seekback,
    deleteInput,
    replaceInput,
    flushInput,

    -- * TODO
    putStr,
    putChar,

    -- * TODO
    test,
    -- sizeofInputBuffer,

    -- * TODO
    InputIO (InputIO),
  )
where

import Control.Exception (assert)

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Primitive (PrimMonad, PrimState, primitive)
import Control.Monad.State.Strict (MonadState, get, put, modify, gets)

import Data.Foldable (traverse_)
import Data.Kind (Type)
import Data.Primitive
  ( MutableByteArray (MutableByteArray),
    copyMutableByteArray,
    fillByteArray,
    getSizeofMutableByteArray,
    mutableByteArrayContents,
    newAlignedPinnedByteArray,
    writeByteArray, indexByteArray, readByteArray
  )

import GHC.Exts (Int (I#), Int#, MutableByteArray#, RealWorld, State#)
import GHC.Exts qualified as GHC
import GHC.IO (IO (IO))
import GHC.IO.Handle qualified as IO

import Prelude hiding (putChar, putStr)

import System.IO qualified as IO
import Foreign.Ptr (plusPtr)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
test :: IO ()
test = do
  ixs <- runInputIO do
    i0 <- get
    putStr "abcefg"
    putStr "abcefg"
    seekback
    replaceInput ' '
    flushInput
    i1 <- get
    pure (i0, i1)
  print ixs

-- | TODO
--
-- @since 1.0.0
runInputIO :: InputIO a -> IO a
runInputIO (InputIO k) =
  IO \st0# ->
    let !(# st1#, dst# #) = GHC.newAlignedPinnedByteArray# 4# 4# st0#
        !(# st2#, _, x #) = k (InputCtx# (# 0# #)) st1#
     in (# GHC.setByteArray# dst# 0# 4# 0# st2#, x #)

-- | TODO
--
-- @since 1.0.0
deleteInput :: InputIO ()
deleteInput = do 
  liftIO $ IO.hPutStr IO.stdout "\ESC[J"
  -- buf <- getInputBuffer
  -- len <- sizeofInputBuffer
  -- pos <- get
  -- seekback
  -- assert (0 <= pos && pos <= len) $ liftIO do 
  --   chr <- if pos < len 
  --     then readByteArray buf $ succ pos
  --     else pure ' '

-- | TODO
--
-- @since 1.0.0
replaceInput :: Char -> InputIO ()
replaceInput chr = putStr ['\b', chr, '\b']

-- | TODO
--
-- @since 1.0.0


-- | TODO
--
-- @since 1.0.0
seek :: InputIO ()
seek = do
  modify (4 *)
  -- len <- sizeofInputBuffer
  -- \pos -> 
  --   if 4 * pos < len 
  --     then 1 + pos 
  --     else pos

-- | TODO
--
-- @since 1.0.0
seekback :: InputIO ()
seekback = do 
  pos <- get
  when (0 < pos) do 
    liftIO $ IO.hPutStr IO.stdout "\ESC[3D"
    liftIO $ IO.hFlush IO.stdout 
    modify (subtract 1)

-- | TODO
--
-- @since 1.0.0
flushInput :: InputIO ()
flushInput = do
  liftIO $ IO.hFlush IO.stdout
  -- buf <- getInputBuffer
  -- len <- sizeofInputBuffer
  -- let ptr = mutableByteArrayContents buf 
  -- liftIO $ IO.hPutBuf IO.stdout ptr len
  --   fillByteArray buf 0 len 0
  -- put 0

-- | TODO
--
-- @since 1.0.0
putStr :: String -> InputIO ()
putStr = traverse_ putChar
{-# INLINE putStr #-}

-- | TODO
--
-- @since 1.0.0
putChar :: Char -> InputIO ()
putChar chr = do
  liftIO $ IO.hPutChar IO.stdout chr 
  seekback
{-# INLINE putChar #-}

-- | TODO
--
-- @since 1.0.0
-- writeout :: Int -> Char -> InputIO ()
-- writeout i chr = assert (0 <= i) do
--   len <- sizeofInputBuffer
--   when (4 * i >= len) (grow 4)
--   buf <- getInputBuffer
--   writeByteArray buf i chr
-- {-# INLINE writeout #-}

-- | TODO
--
-- @since 1.0.0
-- grow :: Int -> InputIO ()
-- grow n = do
--   len <- sizeofInputBuffer
--   src <- getInputBuffer
--   dst <- newAlignedPinnedByteArray (n + len) 4
--   fillByteArray dst 0 len 0
--   copyMutableByteArray dst 0 src 0 len
--   setInputBuffer dst
-- {-# INLINE grow #-}

-- -- | TODO
-- --
-- -- @since 1.0.0
-- setInputBuffer :: MutableByteArray RealWorld -> InputIO ()
-- setInputBuffer (MutableByteArray buf#) =
--   InputIO \ctx# st# -> case ctx# of
--     InputCtx# (# loc# #) -> (# st#, InputCtx# (# buf#, loc# #), () #)
-- {-# INLINE setInputBuffer #-}

-- -- | TODO
-- --
-- -- @since 1.0.0
-- getInputBuffer :: InputIO (MutableByteArray RealWorld)
-- getInputBuffer = InputIO \ctx# st# -> case ctx# of
--   InputCtx# (# buf#, _ #) -> (# st#, ctx#, MutableByteArray buf# #)
-- {-# INLINE getInputBuffer #-}

-- | TODO
--
-- @since 1.0.0
-- sizeofInputBuffer :: InputIO Int
-- sizeofInputBuffer = do
--   buf <- getInputBuffer
--   getSizeofMutableByteArray buf
-- {-# INLINE sizeofInputBuffer #-}

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
newtype InputCtx# = InputCtx# (# Int# #)

-- | TODO
--
-- @since 1.0.0
data InputIO (a :: Type) :: Type where
  InputIO :: (InputCtx# -> State# RealWorld -> (# State# RealWorld, InputCtx#, a #)) -> InputIO a

-- | @since 1.0.0
instance Functor InputIO where
  fmap f (InputIO k) =
    InputIO \ctx0# st0# ->
      let !(# st1#, ctx1#, x #) = k ctx0# st0#
       in (# st1#, ctx1#, f x #)
  {-# INLINE fmap #-}

  x <$ InputIO k = 
    InputIO \ctx0# st0# ->
      let !(# st1#, ctx1#, _ #) = k ctx0# st0#
       in (# st1#, ctx1#, x #)
  {-# INLINE (<$) #-}

-- | @since 1.0.0
instance Applicative InputIO where
  pure x = InputIO \ctx# st# -> (# st#, ctx#, x #)
  {-# INLINE pure #-}

  InputIO f <*> InputIO g =
    InputIO \ctx0# st0# ->
      let !(# st1#, ctx1#, k #) = f ctx0# st0#
          !(# st2#, ctx2#, x #) = g ctx1# st1#
       in (# st2#, ctx2#, k x #)
  {-# INLINE (<*>) #-}

-- | @since 1.0.0
instance Monad InputIO where
  InputIO k >>= f =
    InputIO \ctx0# st0# ->
      let !(# st1#, ctx1#, x #) = k ctx0# st0#
       in case f x of
            InputIO k' -> k' ctx1# st1#
  {-# INLINE (>>=) #-}

-- | @since 1.0.0
instance MonadIO InputIO where
  liftIO (IO k) =
    InputIO \ctx# st0# ->
      let !(# st1#, x #) = k st0#
       in (# st1#, ctx#, x #)
  {-# INLINE liftIO #-}

-- | @since 1.0.0
instance PrimMonad InputIO where
  type PrimState InputIO = RealWorld

  primitive k =
    InputIO \ctx# st0# ->
      let !(# st1#, x #) = k st0#
       in (# st1#, ctx#, x #)
  {-# INLINE primitive #-}

-- | @since 1.0.0
instance MonadState Int InputIO where
  get = InputIO \ctx# st# -> case ctx# of
    InputCtx# (# pos# #) -> (# st#, ctx#, I# pos# #)
  {-# INLINE get #-}

  put (I# pos#) =
    InputIO \(InputCtx# (# _ #)) st# ->
      (# st#, InputCtx# (# pos# #), () #)
  {-# INLINE put #-}
