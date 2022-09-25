
module Data.Parse.Error
  ( -- * TODO
    Error (Error, exn'code, exn'name, exn'begin, exn'end, exn'posn, exn'span),

    -- * TODO
    ErrorSort
      ( ExnEmptyApp,
        ExnChrMatch,
        ExnStrMatch,
        ExnBalance,
        ExnFeedEoF,
        ExnReport
      ),
  )
where

import Data.Kind (Type)
import Data.SrcLoc (SrcLoc)

import Text.Printf (PrintfArg)
import Text.Printf qualified as Text

--------------------------------------------------------------------------------

import Opal.Common.Symbol (Symbol)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data Error = Error
  { exn'code :: ErrorSort
  , exn'name :: String
  , exn'begin :: {-# UNPACK #-} !SrcLoc
  , exn'end :: {-# UNPACK #-} !SrcLoc
  , exn'posn :: {-# UNPACK #-} !SrcLoc
  , exn'span :: Symbol
  }
  deriving (Eq, Ord, Show)

-- | @since 1.0.0
instance PrintfArg Error where
  formatArg exn fmt
    | 'v' == fmtChr = Text.printf "error:%v: %s" (exn'begin exn) (exn'code exn)
    | otherwise = Text.errorBadFormat fmtChr
    where
      fmtChr :: Char
      fmtChr = Text.fmtChar fmt

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
data ErrorSort :: Type where
  -- | TODO
  ExnEmptyApp :: ErrorSort
  -- | TODO
  ExnChrMatch ::
    Maybe Char ->
    {-# UNPACK #-} !Char ->
    ErrorSort
  -- | TODO
  ExnStrMatch :: 
    Maybe String -> 
    String -> 
    ErrorSort
  -- | TODO
  ExnBalance ::
    {-# UNPACK #-} !Char ->
    {-# UNPACK #-} !Char ->
    {-# UNPACK #-} !Char ->
    ErrorSort
  -- | TODO
  ExnFeedEoF :: ErrorSort
  -- | TODO
  ExnReport :: String -> ErrorSort
  deriving (Eq, Ord, Show)

-- | since 1.0.0
instance PrintfArg ErrorSort where
  formatArg exn fmt
    | 'v' == fmtChr = case exn of
        ExnEmptyApp -> showString "#%app: missing procedure expression"
        ExnChrMatch c0 c1 -> showString ("expected character '" ++ show c0 ++ "' but got " ++ show c1)
        ExnStrMatch (Just str0) str1 -> showString ("expected string '" ++ str0 ++ "' but got " ++ show str1)
        ExnStrMatch Nothing str1 -> showString ("unexpected string '" ++ str1 ++ "'")
        ExnBalance c0 c1 c2 -> showString ("imbalanced " ++ show c0 ++ ": no matching '" ++ show c1 ++ "', got" ++ show c2)
        ExnFeedEoF -> showString "end of file"
        ExnReport str -> showString str
    | 's' == fmtChr = shows exn
    | otherwise = Text.errorBadFormat fmtChr
    where
      fmtChr :: Char
      fmtChr = Text.fmtChar fmt
