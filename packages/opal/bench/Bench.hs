
module Main (main) where

import Control.Concurrent.Async (replicateConcurrently_)
import Control.Concurrent.STM (atomically, isFullTBQueue)
import Control.Concurrent.STM.TBQueue (flushTBQueue, newTBQueueIO, writeTBQueue)

import Control.DeepSeq (force)

import Control.Exception (evaluate)

import Control.Monad (when)

import Data.Bool (bool)
import Data.Function (fix)
import Data.Hashable (Hashable (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Word (Word8)

import GHC.Conc (numCapabilities)
import GHC.Stack (HasCallStack)
import GHC.Storable (writeWord8OffPtr)

import Opal.Common.Default (Default (..))
import Opal.Common.Scope (Scope (..))
import Opal.Common.Symbol (Symbol, stringToSymbol, lengthSymbol, symbolToString)
import Opal.Memory.Buffer
  ( Buffer,
    newBuffer,
    fillWord8OffBuffer,
    withBufferContents
  )
import Opal.Memory.Ptr (Ptr, plusPtr)
import Opal.Reader.Monad (ReaderState (..), runReader, whitespace)
import Opal.Writer (Display (..), pretty)

import System.Exit (exitFailure, exitSuccess)
import System.Random (Random (..), newStdGen, setStdGen, randomIO, randomRIO)

import Test.Tasty.Bench
  ( Benchmark
  , bgroup
  , bench
  , env
  , whnf
  , benchIngredients
  )
import Test.Tasty.Runners (parseOptions, tryIngredients)

--------------------------------------------------------------------------------

main :: IO ()
main  = do
  options <- parseOptions benchIngredients benchmarks
  case tryIngredients benchIngredients options benchmarks of
    Nothing -> exitFailure
    Just mb -> mb >>= bool exitFailure exitSuccess

benchmarks :: Benchmark
benchmarks =
  bgroup "bench"
    [ bgroup "reader"
        [ bgroup "whitespace"
            [ env (genReaderState 1028) \s ->
                bench "size: 1028" (whnf (runReader def s) whitespace)
            ]
        ]
    , bgroup "scope"
        [ env genScope (bench "display" . whnf (pretty 0 . display))
        ]
    , bgroup "symbol"
        [ bgroup "display"
            [ env (genSymbol 32) (bench "size: 32" . whnf (pretty 0 . display))
            , env (genSymbol 64) (bench "size: 64" . whnf (pretty 0 . display))
            ]
        , bgroup "stringToSymbol"
            [ env (genString 128) (bench "size: 128" . whnf stringToSymbol)
            , env (genString 256) (bench "size: 256" . whnf stringToSymbol)
            ]
        , bgroup "symbolToString"
            [ env (genSymbol 128) (bench "size: 128" . whnf symbolToString)
            , env (genSymbol 256) (bench "size: 256" . whnf symbolToString)
            ]
        , bgroup "hash"
            [ env (genSymbol 128) (bench "size: 128" . whnf hash)
            , env (genSymbol 256) (bench "size: 256" . whnf hash)
            ]
        , bgroup "lengthSymbol"
            [ env (genSymbol 128) (bench "size: 128" . whnf lengthSymbol)
            , env (genSymbol 256) (bench "size: 256" . whnf lengthSymbol)
            ]
        ]
    ]

genReaderState :: HasCallStack => Int -> IO ReaderState
genReaderState len = do
  buffer <- genWhitespaceBuffer len
  pure def { reader_buffer = buffer }

genScope :: HasCallStack => IO Scope
genScope = fmap Scope (randomRIO (0, maxBound))

genWhitespaceBuffer :: HasCallStack => Int -> IO Buffer
genWhitespaceBuffer len = do
  buffer <- newBuffer len
  fillWord8OffBuffer buffer 0 len spaceWord8

  withBufferContents buffer \begin -> do
    let end :: Ptr Word8
        end = begin `plusPtr` len

    let step :: Ptr Word8 -> IO ()
        step ptr = when (ptr < end) do
          here <- randomIO
          when here (writeWord8OffPtr ptr 0 newlineWord8)
          step (ptr `plusPtr` 1)

    step begin

  pure buffer
  where
    newlineWord8 :: Word8
    newlineWord8 = fromIntegral (fromEnum '\n')

    spaceWord8 :: Word8
    spaceWord8 = fromIntegral (fromEnum ' ')

-- | Creates a 'String' string of randomly generated characters of the
-- specified length.
genSymbol :: HasCallStack => Int -> IO Symbol
genSymbol len = do
  symbol <- fmap stringToSymbol (genString len )
  evaluate (force symbol)

-- | Creates a 'String' string of randomly generated characters of the
-- specified length.
genString :: HasCallStack => Int -> IO String
genString len
  | len < 0   = error ("genSymbolString: arg #1 `length = " ++ show len ++ "' must be non-negative")
  | otherwise = do
    characters <- newTBQueueIO (fromIntegral len)

    forks numCapabilities do
      refStdGen <- newIORef =<< newStdGen

      fix \loop -> do
        stdgen <- readIORef refStdGen
        result <- atomically do
          isFull <- isFullTBQueue characters
          if isFull
            then pure Nothing
            else do
              let (char, stdgen') = random stdgen
              writeTBQueue characters char
              pure (Just stdgen')

        case result of
          Nothing      -> setStdGen stdgen
          Just stdgen' -> writeIORef refStdGen stdgen' >> loop

    result <- atomically (flushTBQueue characters)
    evaluate (force result)

forks :: HasCallStack => Int -> IO () -> IO ()
forks count op
  | count < 1 = error ("forks: arg #1 `count = " ++ show count ++ "' must be >= 1")
  | otherwise = replicateConcurrently_ count op
