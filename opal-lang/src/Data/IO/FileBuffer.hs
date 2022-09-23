module Data.IO.FileBuffer
  ( FileBuffer (FB),
    fromString,
    openFileBufferIO,
    index,
    size,
  )
where


import Data.Kind (Type)
import Data.CharArray.Prim (CharArray#)
import Data.CharArray.Prim qualified as CharArray


import GHC.Exts (Char (C#), Int (I#))

import System.IO qualified as IO

data FileBuffer :: Type where
  FB :: CharArray# -> FileBuffer

fromString :: String -> FileBuffer
fromString str = FB (CharArray.pack# str)

openFileBufferIO :: FilePath -> IO FileBuffer
openFileBufferIO filepath = do
  IO.withFile filepath IO.ReadMode \handle -> do
    !cts <- IO.hGetContents handle
    pure $! FB (CharArray.pack# cts)

index :: FileBuffer -> Int -> Char
index (FB buf#) (I# i#) = C# (CharArray.index# buf# i#)

size :: FileBuffer -> Int 
size (FB buf#) = I# (CharArray.size# buf#)

