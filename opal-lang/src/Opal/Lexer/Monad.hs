{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Lexer.Monad
  ( Lexer (L#, unL#),
    run,

    -- * Error
    raise,
    raiseLoc,

    -- * Context
    scope,
    local,
    askFileBuffer,

    -- * State
    location,
    position,

    -- * Combinators
    discard,
    finalize,
  )
where

import Control.Monad.Error.Class (MonadError, catchError, throwError)
import Control.Monad.State.Class (MonadState, get, put, state)

import Data.IO.FileBuffer (FileBuffer (FB))
import Data.IO.FileBuffer qualified as FileBuffer
import Data.Kind (Type)
import Data.SrcLoc (SrcLoc (SrcLoc))
import Data.SrcLoc.Prim (SrcLoc# (SrcLoc#))
import Data.CharArray.Prim (CharArray#)
import Data.CharArray.Prim qualified as CharArray

import GHC.Exts (Int (I#), Int#)
import GHC.Exts qualified as GHC


--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol (Symbol))

import Opal.Lexer.Error (Error, newError, ErrorSort)
import qualified Data.SrcLoc as SrcLoc

--------------------------------------------------------------------------------

run :: FileBuffer -> Lexer a -> Either Error a
run buf@(FB buf#) (L# k) = do
  let !(I# len#) = FileBuffer.size buf
   in case k buf# len# (SrcLoc# 0# 1# 0#) of
        (# _, (# e | #) #) -> Left e
        (# _, (# | x #) #) -> Right x
{-# INLINE run #-}

newtype Lexer (a :: Type) :: Type where
  L# ::
    { unL# ::
        CharArray# ->
        Int# -> -- The beginning and end of the FileBuffer avaliable to the lexer, and the nesting level
        SrcLoc# -> -- Offset, line, and column being inspected in the FileBuffer
        (# SrcLoc#, (# Error| a #) #)
    } ->
    Lexer a

instance Functor Lexer where
  fmap f (L# k) =
    L# \buf# span# loc0# ->
      case k buf# span# loc0# of
        (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
        (# loc1#, (# | x #) #) -> (# loc1#, (# | f x #) #) 
  {-# INLINE fmap #-}

instance Applicative Lexer where
  pure x = L# \_ _ loc# -> (# loc#, (# | x #) #)
  {-# INLINE pure #-}

  L# f <*> L# g =
    L# \buf# p# loc0# ->
      case f buf# p# loc0# of
        (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
        (# loc1#, (# | k #) #) ->
          case g buf# p# loc1# of
            (# loc2#, (# e | #) #) -> (# loc2#, (# e | #) #)
            (# loc2#, (# | x #) #) -> (# loc2#, (# | k x #) #) 
  {-# INLINE (<*>) #-}

instance Monad Lexer where
  L# k >>= f =
    L# \buf# p# loc0# ->
      case k buf# p# loc0# of
        (# loc1#, (# e | #) #) -> (# loc1#, (# e | #) #)
        (# loc1#, (# | x #) #) -> unL# (f x) buf# p# loc1# 
  {-# INLINE (>>=) #-}

instance MonadError Error Lexer where
  throwError e = L# \_ _ loc# -> (# loc#, (# e | #) #)
  {-# INLINE throwError #-}

  catchError (L# k) f =
    L# \buf# p# loc0# ->
      case k buf# p# loc0# of
        (# loc1#, (# e | #) #) -> unL# (f e) buf# p# loc1# 
        (# loc1#, (# | x #) #) -> (# loc1#, (# | x #) #)
  {-# INLINE catchError #-}

instance MonadState SrcLoc Lexer where
  get =
    L# \_ _ loc@(SrcLoc# x0# y0# z0#) ->
      (# loc, (# | SrcLoc (I# x0#) (I# y0#) (I# z0#) #) #)
  {-# INLINE get #-}

  put (SrcLoc (I# x#) (I# y#) (I# z#)) =
    L# \_ _ _ ->
      (# SrcLoc# x# y# z#, (# | () #) #) 
  {-# INLINE put #-}

  state k =
    L# \_ _ (SrcLoc# x0# y0# z0#) ->
      case k (SrcLoc (I# x0#) (I# y0#) (I# z0#)) of
        (x, SrcLoc (I# x1#) (I# y1#) (I# z1#)) ->
          (# SrcLoc# x1# y1# z1#, (# | x #) #)
  {-# INLINE state #-}

raise :: ErrorSort -> Lexer a
raise msg = raiseLoc msg =<< location
{-# INLINE CONLIKE raise #-}

raiseLoc :: ErrorSort -> SrcLoc -> Lexer a
raiseLoc msg loc = do
  buf <- askFileBuffer
  scp <- scope
  throwError (newError buf loc (scp - loc.posn) msg)
{-# INLINE CONLIKE raiseLoc #-}

scope :: Lexer Int
scope = 
  L# \_ span# srcloc# -> 
    (# srcloc#, (# | I# span# #) #)
{-# INLINE scope #-}

local :: Int -> Lexer a -> Lexer a
local (I# n#) (L# lex#) = L# \bxs# _ -> lex# bxs# n#
{-# INLINE local #-}

position :: Lexer Int 
position = fmap SrcLoc.posn location

location :: Lexer SrcLoc
location = 
  L# \_ _ loc@(SrcLoc# x# y# z#) ->
    (# loc, (# | SrcLoc (I# x#) (I# y#) (I# z#) #) #) 
{-# INLINE location #-}

askFileBuffer :: Lexer FileBuffer
askFileBuffer = L# \bxs# _ loc# -> (# loc#, (# | FB bxs# #) #)
{-# INLINE CONLIKE askFileBuffer #-}

discard :: Lexer ()
discard = pure ()

finalize :: Lexer Symbol
finalize =
  GHC.runRW# \st0# -> 
    L# \src# i0# loc@(SrcLoc# i1# _ _) ->
      let !(# _#, chrs# #) = CharArray.slice# src# i0# i1# st0# 
       in (# loc, (# | Symbol chrs# #) #)