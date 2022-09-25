{-# LANGUAGE OverloadedStrings #-}

module Opal.Reader.Quote
  ( rQuoteForms,
    rUnkownQuoteError,
  
    -- * Quote Readers
    rQuote,
    rQuasi,
    rUnquote,

    -- * Syntax Quote Readers
    rQuoteSyntax,
    rQuasiSyntax,
    rUnquoteSyntax,
  )
where

import Control.Monad.State.Strict (get)

import Data.Functor (($>))

--------------------------------------------------------------------------------

import Data.Parse (Parse, string, single, alt, scope'end, source'span, advanceN, raise, scope)
import Data.Parse.Error (ErrorSort (ExnReport))

import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax (Syntax (Idt), StxCtx (StxCtx), StxIdt (StxIdt))

--------------------------------------------------------------------------------

rQuoteForms :: Parse Syntax 
rQuoteForms = 
   scope get (advanceN 2) do 
    alt rQuote 
      . alt rQuasi 
      . alt rQuasi
      . alt rUnquote
      . alt rQuoteSyntax
      . alt rQuasiSyntax 
      $ alt rUnquoteSyntax rUnkownQuoteError

rUnkownQuoteError :: Parse Syntax
rUnkownQuoteError = do 
  symbol <- scope'end (advanceN 2) source'span
  raise (ExnReport ("unknown quote: " ++ show symbol))

-- Quote Readers ---------------------------------------------------------------

rQuote :: Parse Syntax
rQuote = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 2 MultiScopeSet.empty
   in single '\'' $> Idt (StxIdt stxctx "quote")

rQuasi :: Parse Syntax
rQuasi = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 1 MultiScopeSet.empty
   in single '`' $> Idt (StxIdt stxctx "quasiquote")

rUnquote :: Parse Syntax
rUnquote = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 1 MultiScopeSet.empty
   in single ',' $> Idt (StxIdt stxctx "unquote")

-- Syntax Quote Readers --------------------------------------------------------

rQuoteSyntax :: Parse Syntax
rQuoteSyntax = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 2 MultiScopeSet.empty
   in string "#'" $> Idt (StxIdt stxctx "syntax")

rQuasiSyntax :: Parse Syntax
rQuasiSyntax = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 2 MultiScopeSet.empty
   in string "#`" $> Idt (StxIdt stxctx "quasisyntax")

rUnquoteSyntax :: Parse Syntax
rUnquoteSyntax = do
  loc <- get
  let stxctx :: StxCtx
      stxctx = StxCtx loc 2 MultiScopeSet.empty
   in string "#," $> Idt (StxIdt stxctx "unsyntax")