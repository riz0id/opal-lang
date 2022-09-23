{-# LANGUAGE OverloadedRecordDot #-}

module Opal.Lexer.Error
  ( -- * TODO 
  
    Error (Error, errLoc, errLen, errSort, errSrc),
    newError,
  
    -- * TODO
    ErrorSort (ExnEmptyApp, ExnChrMatch, ExnStrMatch, ExnReport),
  )
where

import Data.IO.FileBuffer (FileBuffer)
import Data.IO.FileBuffer qualified as FileBuffer
import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)

import Text.Printf (PrintfArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data Error = Error
  { errLoc :: {-# UNPACK #-} !SrcLoc
  , errLen :: {-# UNPACK #-} !Int
  , errSort :: ErrorSort
  , errSrc :: String
  }
  deriving (Eq, Ord, Show)

-- | @since 1.0.0
instance PrintfArg Error where
  formatArg exn fmt
    | 'v' == fmtChr = Text.printf "error:%v: %s" (errLoc exn) (errSort exn)
    | otherwise = Text.errorBadFormat fmtChr
    where
      fmtChr :: Char
      fmtChr = Text.fmtChar fmt

-- | TODO 
--
-- @since 1.0.0
newError :: FileBuffer -> SrcLoc -> Int -> ErrorSort -> Error
newError buffer loc len msg =
  let src :: String
      src = foldr ((:) . FileBuffer.index buffer) "" [loc.posn .. (loc.posn + len) - 1]
   in Error loc len msg src

--------------------------------------------------------------------------------

-- | TODO 
--
-- @since 1.0.0
data ErrorSort :: Type where
  -- | TODO 
  ExnEmptyApp :: ErrorSort
  -- | TODO 
  ExnChrMatch :: {-# UNPACK #-} !Char -> ErrorSort
  -- | TODO 
  ExnStrMatch :: String -> ErrorSort
  -- | TODO
  ExnReport :: String -> ErrorSort
  deriving (Eq, Ord, Show)

-- | since 1.0.0
instance PrintfArg ErrorSort where
  formatArg exn fmt
    | 'v' == fmtChr = case exn of
        ExnEmptyApp -> showString "#%app: missing procedure expression"
        ExnChrMatch chr -> showString ("expected character '" ++ show chr ++ "'") 
        ExnStrMatch str -> showString ("expected string '" ++ str ++ "'")
        ExnReport str -> showString str 
    | 's' == fmtChr = shows exn
    | otherwise = Text.errorBadFormat fmtChr
    where
      fmtChr :: Char
      fmtChr = Text.fmtChar fmt
