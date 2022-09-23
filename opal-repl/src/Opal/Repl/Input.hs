{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Opal.Repl.Input
  ( inputIO,
    rawInputIO,
  )
where

import Data.Function (fix)
import Data.Functor (void)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)

import Control.Monad (when, foldM, unless)
import System.IO qualified as IO

--------------------------------------------------------------------------------

import Opal.Repl.Input.Monad (InputIO, runInputIO)
import Opal.Repl.Input.Monad qualified as Input
import Control.Monad.IO.Class (liftIO)
import Data.Primitive (MutableByteArray, newAlignedPinnedByteArray, readMutVar, getSizeofMutableByteArray, writeByteArray, mutableByteArrayContents, freezeByteArray, foldrByteArray, fillByteArray, resizeMutableByteArray, writeMutVar)
import Control.Monad.Primitive (RealWorld)
import Data.Primitive.MutVar (MutVar, newMutVar)
import Control.Exception (assert)
import Text.Read (Lexeme(String))

--------------------------------------------------------------------------------

type Buffer = MutVar RealWorld (MutableByteArray RealWorld)

newBuffer :: IO Buffer 
newBuffer = do 
  buf <- newAlignedPinnedByteArray 32 4
  fillByteArray buf 0 32 0
  newMutVar buf  

sizeofBuffer :: Buffer -> IO Int
sizeofBuffer ref = do 
  buf <- readMutVar ref
  len <- getSizeofMutableByteArray buf
  pure (4 * len)

growBuffer :: Buffer -> IO ()
growBuffer ref = do 
  old <- readMutVar ref
  len <- sizeofBuffer ref
  new <- resizeMutableByteArray old (4 + len)
  writeMutVar ref new

writeBuffer :: Buffer -> Int -> Char -> IO ()
writeBuffer ref i x = do 
  len <- sizeofBuffer ref
  when (i >= len) (growBuffer ref)
  buf <- readMutVar ref
  writeByteArray buf i x

writeBufferOffString :: Buffer -> Int -> String -> IO ()
writeBufferOffString buf = run
  where 
    run :: Int -> String -> IO ()
    run _ "" = pure ()
    run i (c : cs) = writeBuffer buf i c >> run (1 + i) cs

bufferToString :: Buffer -> Int -> IO String
bufferToString ref len = do 
  buf <- readMutVar ref 
  bxs <- freezeByteArray buf 0 len 
  pure (foldrByteArray (:) "" bxs)

putBuffer :: Buffer -> Int -> IO () 
putBuffer ref len = do 
  buf <- readMutVar ref
  let ptr = mutableByteArrayContents buf
  IO.hPutBuf IO.stdout ptr (4 * len)

inputIO :: IO String
inputIO = do
  bufRef <- newBuffer 
  posRef <- newIORef @Int 0
  lenRef <- newIORef @Int 0

  fix \next -> do
    void (IO.hWaitForInput IO.stdin -1)
    input <- rawInputIO

    case input of
      "\DEL" -> do 
        _
      "\n" -> do 
        pos <- readIORef posRef
        bufferToString bufRef pos
      _ -> do 
        pos <- readIORef posRef
        modifyIORef' posRef (length input +)
        writeBufferOffString bufRef pos input
        IO.hPutStr IO.stdout input
        IO.hFlush IO.stdout 
        next
      -- "\ESC[C" -> do 
      --   IO.hPutStr IO.stdout "\ESC[C"
      --   IO.hFlush IO.stdout 
      --   n <- sizeofBuffer buf
      --   modifyIORef' cursorRef (min n . succ)
      --   next
      -- "\ESC[D" -> do 
      --   IO.hPutStr IO.stdout "\ESC[D"
      --   IO.hFlush IO.stdout 
      --   modifyIORef' cursorRef (max 0 . pred)
      --   next
      -- "\DEL" -> do 
      --   pos <- readIORef cursorRef
      --   buf <- readIORef bufferRef
      --   let buffer' = drop pos buf
      --   modifyIORef' cursorRef (max 0 . pred)
      --   IO.hPutStr IO.stdout "\ESC[D\ESC[0K"
      --   IO.hFlush IO.stdout 
      --   IO.hPutStr IO.stdout buffer'
      --   IO.hFlush IO.stdout 
      --   next
      -- "\n" -> do 
      --   IO.hFlush IO.stdout 
      --   readIORef bufferRef
      -- _ -> do 
        -- pos <- readIORef cursorRef
        -- buf <- readIORef bufferRef

        -- let (prefix, suffix) = splitAt pos buf 
        -- modifyIORef' cursorRef succ
        -- writeIORef bufferRef (prefix ++ input ++ suffix)

        -- IO.hPutStr IO.stdout (input ++ suffix)
        
        -- let diff = length buf - pos
        -- when (0 < diff) do
        --   IO.hPutStr IO.stdout ("\ESC[" ++ show diff ++ "D")
        -- IO.hFlush IO.stdout 
        -- next

rawInputIO :: IO String
rawInputIO = do
  void (IO.hWaitForInput IO.stdin -1)
  buffer <- newIORef ""

  fix \next -> do
    ready <- IO.hReady IO.stdin
    when ready do
      input <- IO.hGetChar IO.stdin
      modifyIORef' buffer (++ [input])
      next

  readIORef buffer