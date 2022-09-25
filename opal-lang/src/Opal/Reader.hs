module Opal.Reader
  ( -- * TOD
    rSyntax,
    rAtomSyntax,
    rLiteralSyntax,
    rIdtSyntax,
    rAppSyntax,

    -- * TODO
  )
where

import Control.Monad.State.Strict (get, gets)

import Data.Char (isSpace)
import Data.Functor (($>))
import Data.SrcLoc qualified as SrcLoc

--------------------------------------------------------------------------------

import Data.Parse

import Opal.Common.Symbol qualified as Symbol
import Opal.Expand.Syntax (StxCtx (StxCtx), StxIdt (StxIdt), Syntax (App, Idt))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Reader.Bool (rLiteralBool)
import Opal.Reader.Quote (rQuoteForms)

--------------------------------------------------------------------------------

  
--------------------------------------------------------------------------------

rSyntax :: Parse Syntax
rSyntax = do 
  whitespace
  peek >>= \case
    '\'' -> rQuotedSyntax
    '#' -> alt rLiteralSyntax rQuotedSyntax
    '(' -> rAppSyntax
    '[' -> rAppSyntax
    _ -> rAtomSyntax

rQuotedSyntax :: Parse Syntax
rQuotedSyntax = do
  loc0 <- get
  idt <- rQuoteForms
  stx <- rAtomSyntax
  loc1 <- gets SrcLoc.posn
  let ctx :: StxCtx
      ctx = StxCtx loc0 (loc1 - SrcLoc.posn loc0) MultiScopeSet.empty
   in pure (App ctx [idt, stx])

rAtomSyntax :: Parse Syntax
rAtomSyntax = alt rIdtSyntax rAppSyntax

rLiteralSyntax :: Parse Syntax
rLiteralSyntax = rLiteralBool

rIdtSyntax :: Parse Syntax
rIdtSyntax =
  token $ label "symbol" do
    scope get (scan (not . isSpace) get) do
      loc <- get
      sym <- takeWhile1 isSymbolChar
      let len = Symbol.size sym
          ctx = StxCtx loc len MultiScopeSet.empty
       in advanceN len $> Idt (StxIdt ctx sym)
  where
    isSymbolChar :: Char -> Bool
    isSymbolChar chr = not (isSpace chr || elem chr "#()[]{}`,\'\"\NUL")

rAppSyntax :: Parse Syntax
rAppSyntax = do
  token $ label "procedure application" do
    loc0 <- get
    stxs <-
      peek >>= \case
        '(' -> parens rInnerSyntax
        '[' -> bracks rInnerSyntax
        _ -> raise (ExnReport ("expected application form (stx ...) or [stx ...]"))
    loc1 <- gets SrcLoc.posn
    let ctx :: StxCtx
        ctx = StxCtx loc0 (loc1 - SrcLoc.posn loc0) MultiScopeSet.empty
     in pure (App ctx stxs)
  where
    rInnerSyntax :: Parse [Syntax]
    rInnerSyntax = whitespace *> star rSyntax <* whitespace

-- | TODO
--
-- @since 1.0.0
token :: Parse a -> Parse a
token parse = scope'begin get (parse <* whitespace)
{-# INLINE token #-}