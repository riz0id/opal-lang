{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module Opal.Read (
  -- * TODO
  runRead,

  -- * TODO
  rTopLevelSyntax,
  rStxPrimFalse,
  rStxPrimTrue,
) where

import Control.Applicative (many, some, (<|>))

import Data.SrcLoc (SrcLoc)
import Data.Text (Text)

import Prelude hiding (Read)

import Text.Parsel (Grammar, ParseError)
import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Common.Symbol qualified as Symbol

import Opal.Expand.Syntax (
  StxCtx (StxCtx),
  Syntax (StxAtom, StxBool, StxList, StxPair, StxVoid),
 )
import Opal.Expand.Syntax.MultiScopeSet (MultiScopeSet, Phase (Phase))
import Opal.Expand.Syntax.MultiScopeSet qualified as MultiScopeSet
import Opal.Expand.Syntax.ScopeSet (ScopeId (ScopeId))

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
makeStxCtx :: SrcLoc -> StxCtx
makeStxCtx loc =
  let coreScopes :: MultiScopeSet
      coreScopes = MultiScopeSet.singleton (Phase 0) [ScopeId 0]
   in StxCtx (Just loc) coreScopes

-- | TODO
--
-- @since 1.0.0
makeStxVoid :: SrcLoc -> Syntax
makeStxVoid loc = StxVoid (makeStxCtx loc)

-- | TODO
--
-- @since 1.0.0
makeStxBool :: SrcLoc -> Bool -> Syntax
makeStxBool loc = StxBool (makeStxCtx loc)

-- | TODO
--
-- @since 1.0.0
makeStxPair :: SrcLoc -> Syntax -> Syntax -> Syntax
makeStxPair loc = StxPair (makeStxCtx loc)

-- | TODO
--
-- @since 1.0.0
makeStxAtom :: SrcLoc -> String -> Syntax
makeStxAtom loc symbol = StxAtom (makeStxCtx loc) (Symbol.pack symbol)

-- | TODO
--
-- @since 1.0.0
makeStxList :: SrcLoc -> [Syntax] -> Syntax
makeStxList loc = StxList (makeStxCtx loc)

-- TODO ------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rTopLevelSyntax :: Grammar Syntax
rTopLevelSyntax = do
  Parsel.whitespaces
  loc <- Parsel.location
  stxs <- many rSyntax
  pure (makeStxList loc stxs)

-- | TODO
--
-- @since 1.0.0
rSyntax :: Grammar Syntax
rSyntax = Parsel.whitespaces *> Parsel.choice @[] [rStxPrim, rStxAtom, rStxList]

-- | TODO
--
-- @since 1.0.0
rStxPrim :: Grammar Syntax
rStxPrim =
  Parsel.whitespaces
    *> Parsel.choice
      @[]
      [ rStxPrimVoid
      , rStxPrimFalse
      , rStxPrimTrue
      , rStxPrimQuote
      , rStxPrimUnquote
      , rStxPrimSyntax
      , rStxPrimQuasiSyntax
      , rStxPrimUnsyntax
      , rStxPrimPair
      ]

-- | TODO
--
-- @since 1.0.0
rStxAtom :: Grammar Syntax
rStxAtom = do
  srcloc <- Parsel.location
  symbol <- some (foldr @[] ((<|>) . Parsel.char) Parsel.alphaNum "-_/!+-<>#=*?")
  pure (makeStxAtom srcloc symbol)

-- | TODO
--
-- @since 1.0.0
rStxList :: Grammar Syntax
rStxList = do
  loc <- Parsel.location
  many Parsel.whitespace
  stxs <- rList $ rSurround (many Parsel.whitespace) $ many rSyntax
  pure (makeStxList loc stxs)
  where
    rList :: Grammar a -> Grammar a
    rList tok = Parsel.parentheses tok <|> Parsel.brackets tok

    rSurround :: Grammar x -> Grammar a -> Grammar a
    rSurround a x = a *> x <* a

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rStxPrimVoid :: Grammar Syntax
rStxPrimVoid = do
  srcloc <- Parsel.location
  Parsel.string "#<void>"
  pure (makeStxVoid srcloc)

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
  Parsel.string "#t" <|> Parsel.string "#T"
  pure (makeStxBool srcloc True)

-- | TODO
--
-- @since 1.0.0
rStxPrimPair :: Grammar Syntax
rStxPrimPair = do
  Parsel.char '('
  Parsel.whitespaces
  lhs <- rSyntax
  loc <- Parsel.location
  Parsel.string " . "
  rhs <- rSyntax
  Parsel.whitespaces
  Parsel.char ')'
  pure (makeStxPair loc lhs rhs)

--------------------------------------------------------------------------------

-- | TODO
--
-- @since 1.0.0
rStxPrimQuote :: Grammar Syntax
rStxPrimQuote = do
  loc <- Parsel.location
  Parsel.char '\''
  stx <- rSyntax
  pure (makeStxList loc [makeStxAtom loc "quote", stx])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnquote :: Grammar Syntax
rStxPrimUnquote = do
  loc <- Parsel.location
  Parsel.string "#,"
  stx <- rSyntax
  pure (makeStxList loc [makeStxAtom loc "unquote", stx])

-- | TODO
--
-- @since 1.0.0
rStxPrimSyntax :: Grammar Syntax
rStxPrimSyntax = do
  loc <- Parsel.location
  Parsel.string "#'"
  stx <- rSyntax
  pure (makeStxList loc [makeStxAtom loc "syntax", stx])

-- | TODO
--
-- @since 1.0.0
rStxPrimQuasiSyntax :: Grammar Syntax
rStxPrimQuasiSyntax = do
  loc <- Parsel.location
  Parsel.string "#`"
  stx <- rSyntax
  pure (makeStxList loc [makeStxAtom loc "quasisyntax", stx])

-- | TODO
--
-- @since 1.0.0
rStxPrimUnsyntax :: Grammar Syntax
rStxPrimUnsyntax = do
  loc <- Parsel.location
  Parsel.string "#,"
  stx <- rSyntax
  pure (makeStxList loc [makeStxAtom loc "unsyntax", stx])
