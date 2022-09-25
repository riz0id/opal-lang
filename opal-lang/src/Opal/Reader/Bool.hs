{-# LANGUAGE OverloadedStrings #-}

module Opal.Reader.Bool
  ( -- * Quote Readers
    rLiteralBool,
    rLiteralTrue,
    rLiteralFalse,
  )
where

import Control.Monad.State.Strict (get)

import Prelude hiding (take)

--------------------------------------------------------------------------------

import Data.Parse (Parse, take, single, scope, advanceN, raise, alt)
import Data.Parse.Error (ErrorSort(ExnReport))

import Opal.AST.Literal (Literal(BoolLit))

import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax (Syntax (Lit), StxCtx (StxCtx))

-- Quote Readers ---------------------------------------------------------------

rLiteralBool :: Parse Syntax
rLiteralBool = alt rLiteralTrue rLiteralFalse

rLiteralTrue :: Parse Syntax
rLiteralTrue = do
  scope get (advanceN 2) do 
    loc <- get
    chr <- single '#' *> take
    if chr == 'T' || chr == 't'
      then pure (Lit (StxCtx loc 2 MultiScopeSet.empty) $ BoolLit True)
      else raise (ExnReport ("unknown data literal " ++ ['#', chr]))

rLiteralFalse :: Parse Syntax
rLiteralFalse = do
  scope get (advanceN 2) do 
    loc <- get
    chr <- single '#' *> take
    if chr == 'f' || chr == 'f'
      then pure (Lit (StxCtx loc 2 MultiScopeSet.empty) $ BoolLit False)
      else raise (ExnReport ("unknown data literal " ++ ['#', chr]))
