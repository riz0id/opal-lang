module Data.IO.FileBuffer
  ( FileBuffer (FB),
    fromString,
    openFileBufferIO,
    index,
    size,
  )
where

import Data.ByteArray.Prim (ByteArray#)
import Data.ByteArray.Prim qualified as ByteArray
import Data.Kind (Type)

import GHC.Exts (Int (I#))
import GHC.Word (Word8 (W8#))

import System.IO qualified as IO

data FileBuffer :: Type where
  FB :: ByteArray# -> FileBuffer

fromString :: String -> FileBuffer
fromString str = FB (ByteArray.pack# (map (fromIntegral . fromEnum) str))

openFileBufferIO :: FilePath -> IO FileBuffer
openFileBufferIO filepath = do
  IO.withFile filepath IO.ReadMode \handle -> do
    !cts <- IO.hGetContents handle
    pure $! fromString cts

index :: FileBuffer -> Int -> Char
index (FB buf#) (I# i#) = toEnum (fromIntegral (W8# (ByteArray.index# buf# i#)))

size :: FileBuffer -> Int
size (FB buf#) = I# (ByteArray.size# buf#)
