{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Opal.Read
  ( -- * TODO
    runRead,

    -- * TODO
    rTopLevelSyntax,
    rStxPrimFalse,
    rStxPrimTrue,
  )
where

import Control.Applicative (many, some, (<|>))

import Data.SrcLoc (SrcLoc)
import Data.SrcLoc qualified as SrcLoc
import Data.Text (Text)

import Prelude hiding (Read)

import Text.Parsel (Grammar, ParseError)
import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (StxCtx (StxCtx), Syntax (StxAtom, StxList, StxBool))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
runRead :: Text -> Either ParseError Syntax
runRead src = Parsel.parse src rSyntax

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
makeStxBool :: SrcLoc -> Bool -> Syntax
makeStxBool srcloc bool =
  let context :: StxCtx
      context = StxCtx srcloc 2 MultiScopeSet.empty
   in StxBool context bool 

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
rTopLevelSyntax :: Grammar Syntax
rTopLevelSyntax = do 
  Parsel.whitespaces 
  loc0 <- Parsel.location
  stxs <- many rSyntax 
  loc1 <- Parsel.location
  pure (makeStxList loc0 loc1 stxs)

-- | TODO
--
-- @since 1.0.0
rSyntax :: Grammar Syntax
rSyntax = do 
  Parsel.whitespaces
  stx <- Parsel.choice [rStxPrim, rStxAtom, rStxList]
  Parsel.whitespaces 
  pure stx

-- | TODO
--
-- @since 1.0.0
rStxPrim :: Grammar Syntax
rStxPrim =
  Parsel.choice
    [ rStxPrimFalse
    , rStxPrimTrue
    , rStxPrimQuote
    , rStxPrimUnquote
    , rStxPrimSyntax
    , rPrimQuasiSyntax
    , rStxPrimUnsyntax
    ]

-- | TODO
--
-- @since 1.0.0
rStxPrimFalse :: Grammar Syntax
rStxPrimFalse = do
  srcloc <- Parsel.location
  Parsel.string "#f" <|> Parsel.string "#F"
  pure (makeStxBool srcloc False)

-- | TODO
--
-- @since 1.0.0
rStxPrimTrue :: Grammar Syntax
rStxPrimTrue = do
  srcloc <- Parsel.location
  Parsel.string "#t" <|> Parsel.string "#t"
  pure (makeStxBool srcloc True)

-- | TODO
--
-- @since 1.0.0
rStxPrimQuote :: Grammar Syntax
rStxPrimQuote = do
  begin <- Parsel.location
  quote <- rPrimQuote
  syntax <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [quote, syntax])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnquote :: Grammar Syntax
rStxPrimUnquote = do
  begin <- Parsel.location
  unquote <- rPrimUnquote
  syntax <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [unquote, syntax])

-- | TODO
--
-- @since 1.0.0
rStxPrimSyntax :: Grammar Syntax
rStxPrimSyntax = do
  begin <- Parsel.location
  syntax <- rPrimSyntax
  sexps <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [syntax, sexps])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnsyntax :: Grammar Syntax
rStxPrimUnsyntax = do
  begin <- Parsel.location
  unsyntax <- rPrimUnsyntax
  sexps <- rSyntax
  end <- Parsel.location
  pure (makeStxList begin end [unsyntax, sexps])

-- | TODO
--
-- @since 1.0.0
rStxAtom :: Grammar Syntax
rStxAtom = do
  srcloc <- Parsel.location
  symbol <- some (foldr @[] ((<|>) . Parsel.char) Parsel.alphaNum "-_/!+-<>#=*")
  many Parsel.whitespace
  pure (makeStxAtom srcloc symbol)

-- | TODO
--
-- @since 1.0.0
rStxList :: Grammar Syntax
rStxList = do
  begin <- Parsel.location
  stxs <- rList $ rSurround (many Parsel.whitespace) $ many rSyntax
  end <- Parsel.location
  many Parsel.whitespace
  pure (makeStxList begin end stxs)
  where
    rList :: Grammar a -> Grammar a
    rList tok = Parsel.parentheses tok <|> Parsel.brackets tok

    rSurround :: Grammar x -> Grammar a -> Grammar a
    rSurround a x = a *> x <* a

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rPrimQuote :: Grammar Syntax
rPrimQuote = do
  srcloc <- Parsel.location
  Parsel.char '\''
  pure (makeStxAtom srcloc "quote")

-- | TODO
--
-- @since 1.0.0
rPrimUnquote :: Grammar Syntax
rPrimUnquote = do
  srcloc <- Parsel.location
  Parsel.char ','
  pure (makeStxAtom srcloc "unquote")

-- | TODO
--
-- @since 1.0.0
rPrimSyntax :: Grammar Syntax
rPrimSyntax = do
  srcloc <- Parsel.location
  Parsel.string "#'"
  pure (makeStxAtom srcloc "syntax")

-- | TODO
--
-- @since 1.0.0
rPrimQuasiSyntax :: Grammar Syntax
rPrimQuasiSyntax = do
  srcloc <- Parsel.location
  Parsel.string "#`"
  pure (makeStxAtom srcloc "quasisyntax")

-- | TODO
--
-- @since 1.0.0
rPrimUnsyntax :: Grammar Syntax
rPrimUnsyntax = do
  srcloc <- Parsel.location
  Parsel.string "#,"
  pure (makeStxAtom srcloc "unsyntax")
