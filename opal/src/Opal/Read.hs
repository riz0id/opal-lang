{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}

module Opal.Read
  ( -- * TODO
    runRead,
  )
where

import Control.Applicative (many, some, (<|>))

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc

import Prelude hiding (Read)

import Text.Parsel (Parse, ParseError)
import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (StxCtx (StxCtx), Syntax (StxAtom, StxList))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runRead :: String -> Either ParseError Syntax
runRead src = Parsel.parse src rSyntax

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeStxAtom :: SrcLoc -> String -> Syntax
makeStxAtom srcloc symbol =
  let context :: StxCtx
      context = StxCtx srcloc (length symbol) MultiScopeSet.empty
   in StxAtom context (Symbol.pack symbol)

-- | TODO
--
-- @since 1.0.0
makeStxList :: SrcLoc -> SrcLoc -> [Syntax] -> Syntax
makeStxList begin end stxs =
  let context :: StxCtx
      context = StxCtx begin (SrcLoc.diff begin end) MultiScopeSet.empty
   in StxList context stxs

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rSyntax :: Parse Syntax
rSyntax = many Parsel.whitespace *> Parsel.choice [rStxPrim, rStxAtom, rStxList]

-- | TODO
--
-- @since 1.0.0
rStxPrim :: Parse Syntax
rStxPrim =
  Parsel.choice
    [ rStxPrimFalse
    , rStxPrimTrue
    , rStxPrimQuote
    , rStxPrimUnquote
    , rStxPrimSyntax
    , rStxPrimUnsyntax
    ]

-- | TODO
--
-- @since 1.0.0
rStxPrimFalse :: Parse Syntax
rStxPrimFalse = do
  srcloc <- Parsel.location
  symbol <- Parsel.string "#f"
  pure (makeStxAtom srcloc symbol)

-- | TODO
--
-- @since 1.0.0
rStxPrimTrue :: Parse Syntax
rStxPrimTrue = do
  srcloc <- Parsel.location
  symbol <- Parsel.string "#t"
  pure (makeStxAtom srcloc symbol)

-- | TODO
--
-- @since 1.0.0
rStxPrimQuote :: Parse Syntax
rStxPrimQuote = do
  begin <- Parsel.location
  quote <- pQuote
  syntax <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [quote, syntax])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnquote :: Parse Syntax
rStxPrimUnquote = do
  begin <- Parsel.location
  unquote <- pUnquote
  syntax <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [unquote, syntax])

-- | TODO
--
-- @since 1.0.0
rStxPrimSyntax :: Parse Syntax
rStxPrimSyntax = do
  begin <- Parsel.location
  syntax <- pSyntax
  sexps <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [syntax, sexps])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnsyntax :: Parse Syntax
rStxPrimUnsyntax = do
  begin <- Parsel.location
  unsyntax <- pUnsyntax
  sexps <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [unsyntax, sexps])

-- | TODO
--
-- @since 1.0.0
rStxAtom :: Parse Syntax
rStxAtom = do
  srcloc <- Parsel.location
  symbol <- some (foldr ((<|>) . Parsel.char) Parsel.alphaNum "-/!")
  many Parsel.whitespace
  pure (makeStxAtom srcloc symbol)

-- | TODO
--
-- @since 1.0.0
rStxList :: Parse Syntax
rStxList = do
  begin <- Parsel.location
  stxs <- rList $ Parsel.surround (many Parsel.whitespace) $ many rSyntax
  end <- Parsel.location
  many Parsel.whitespace
  pure (makeStxList begin end stxs)
  where 
    rList :: Parse a -> Parse a 
    rList tok = Parsel.parentheses tok <|> Parsel.brackets tok

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
pQuote :: Parse Syntax
pQuote = do
  srcloc <- Parsel.location
  Parsel.char '\''
  pure (makeStxAtom srcloc "quote")

-- | TODO
--
-- @since 1.0.0
pUnquote :: Parse Syntax
pUnquote = do
  srcloc <- Parsel.location
  Parsel.char ','
  pure (makeStxAtom srcloc "unquote")

-- | TODO
--
-- @since 1.0.0
pSyntax :: Parse Syntax
pSyntax = do
  srcloc <- Parsel.location
  Parsel.string "#'"
  pure (makeStxAtom srcloc "syntax")

-- | TODO
--
-- @since 1.0.0
pUnsyntax :: Parse Syntax
pUnsyntax = do
  srcloc <- Parsel.location
  Parsel.string "#,"
  pure (makeStxAtom srcloc "unsyntax")
