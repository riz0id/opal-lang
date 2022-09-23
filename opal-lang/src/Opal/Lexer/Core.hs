module Opal.Lexer.Core
  ( between,
    feed,
    peek,
    satisfy,
    scan,
    scan1,
    single,
    string,
    whitespace,
  )
where

import Control.Monad (unless)
import Control.Monad.State.Class (gets, modify)

import Data.Char (isSpace)
import Data.Function (fix)
import Data.IO.FileBuffer qualified as FileBuffer
import Data.SrcLoc (posn)
import Data.SrcLoc qualified as SrcLoc

import Opal.Lexer.Error (ErrorSort (ExnChrMatch, ExnStrMatch))
import Opal.Lexer.Monad (Lexer, raise, location, raiseLoc)
import Opal.Lexer.Monad qualified as Monad

--------------------------------------------------------------------------------

between :: Char -> Char -> Lexer a -> Lexer a
between chr0 chr1 lexer = do
  single chr0
  result <- lexer
  single chr1
  pure result
{-# INLINE between #-}

feed :: Char -> Lexer ()
feed chr = modify (`SrcLoc.feed` chr)
{-# INLINE feed #-}

peek :: Lexer Char
peek = do
  pos <- gets posn
  buf <- Monad.askFileBuffer
  if pos < FileBuffer.size buf
    then pure (FileBuffer.index buf pos)
    else pure '\0'
{-# INLINE peek #-}

satisfy :: (Char -> Bool) -> Lexer ()
satisfy p = do
  chr <- peek
  if p chr
    then feed chr
    else raise (ExnChrMatch chr)
{-# INLINE satisfy #-}

scan :: (Char -> Bool) -> Lexer a -> Lexer a
scan p lexer = do
  pos0 <- gets posn
  len0 <- Monad.scope
  fix \next -> do
    chr <- peek
    pos <- gets posn
    if p chr && pos < len0
      then feed chr >> next
      else Monad.local pos0 lexer
{-# INLINE scan #-}

scan1 :: (Char -> Bool) -> Lexer a -> Lexer a 
scan1 p lexer = scan p do 
  i0 <- Monad.scope
  i1 <- Monad.position
  if i1 - i0 < 1 
    then raise . ExnStrMatch . show =<< Monad.finalize
    else lexer
  
single :: Char -> Lexer ()
single chr0 = do
  chr1 <- peek
  if chr0 == chr1
    then feed chr1
    else raise (ExnChrMatch chr1)
{-# INLINE single #-}

string :: String -> Lexer ()
string match = do 
  loc <- location
  str <- foldr con (pure "") match
  unless (str == match) do 
    raiseLoc (ExnStrMatch str) loc
  where 
    con :: Char -> Lexer String -> Lexer String
    con chr0 chrs = do
      chr1 <- peek
      if chr0 == chr1
        then feed chr1 >> fmap (chr0 :) chrs
        else pure [chr1] 
{-# INLINE string #-}

whitespace :: Lexer ()
whitespace = scan isSpace Monad.discard
{-# INLINE whitespace #-}