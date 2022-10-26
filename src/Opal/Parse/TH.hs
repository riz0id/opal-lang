{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Opal.Parse.TH
  ( -- * Quasi Quoters
    syntax, 
    parseSyntaxQuoter,
    parseSyntaxQuoterE,
    quasiSyntaxToPat,

    -- * QuasiSyntax
    QuasiSyntax (..),

    -- * Re-exports
    QuasiQuoter (..), 
  ) 
where

import Control.Applicative (many)

import Data.Data (Data)
import qualified Data.Text as Text

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH (Q, Exp, Pat)
import Language.Haskell.TH.Syntax (Lift, lift)

import Text.Parsel (Grammar)
import Text.Parsel qualified as Parsel
import qualified Language.Haskell.TH as TH

--------------------------------------------------------------------------------

import Opal.Expand.Syntax qualified as Syntax

--------------------------------------------------------------------------------

syntax :: QuasiQuoter
syntax = 
  QuasiQuoter 
    { quoteExp = parseSyntaxQuoterE
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
    }

data QuasiSyntax
  = QuasiSyntaxAtom String
  | QuasiSyntaxList [QuasiSyntax]
  | QuasiSyntaxMany QuasiSyntax
  | QuasiSyntaxSome QuasiSyntax
  deriving (Data, Eq, Ord, Lift, Show)

parseSyntaxQuoterE :: String -> Q Exp
parseSyntaxQuoterE src = parseSyntaxQuoter src >>= lift

parseSyntaxQuoterPat :: String -> Q Exp
parseSyntaxQuoterPat src = parseSyntaxQuoter src >>= lift

quasiSyntaxToPat :: QuasiSyntax -> Q Pat
quasiSyntaxToPat = \case 
  QuasiSyntaxAtom atom -> do 
    pure (TH.ConP 'Syntax.StxAtom [] [TH.WildP, TH.VarP (TH.mkName atom)])
  QuasiSyntaxList stxs -> do 
    pats <- TH.listP (map quasiSyntaxToPat stxs)
    pure (TH.ConP 'Syntax.StxList [] [TH.WildP, pats])
  QuasiSyntaxMany qstx -> undefined 
  QuasiSyntaxSome qstx -> undefined

parseSyntaxQuoter :: String -> Q QuasiSyntax
parseSyntaxQuoter src = 
  case Parsel.parse (Text.pack src) pQuasiSyntax of 
    Left exn -> fail (show exn)
    Right rx -> pure rx

pQuasiSyntax :: Grammar QuasiSyntax 
pQuasiSyntax = 
  Parsel.choice
    [ pQuasiSyntaxAtom
    , pQuasiSyntaxList
    , pQuasiSyntaxMany
    , pQuasiSyntaxSome
    ]

pQuasiSyntaxAtom :: Grammar QuasiSyntax 
pQuasiSyntaxAtom = do 
  Parsel.whitespaces
  char <- Parsel.alpha 
  rest <- many Parsel.alphaNum
  Parsel.whitespaces
  pure (QuasiSyntaxAtom (char : rest))

pQuasiSyntaxList :: Grammar QuasiSyntax 
pQuasiSyntaxList = do 
  Parsel.whitespaces
  Parsel.char '('
  Parsel.whitespaces
  stxs <- many pQuasiSyntax
  Parsel.whitespaces
  Parsel.char ')'
  Parsel.whitespaces
  pure (QuasiSyntaxList stxs)

pQuasiSyntaxMany :: Grammar QuasiSyntax 
pQuasiSyntaxMany = do 
  Parsel.whitespaces
  Parsel.char '('
  stx <- pQuasiSyntax
  Parsel.string "...)"
  Parsel.whitespaces
  pure (QuasiSyntaxMany stx)

pQuasiSyntaxSome :: Grammar QuasiSyntax 
pQuasiSyntaxSome = do 
  Parsel.whitespaces
  Parsel.char '('
  Parsel.whitespaces
  stx <- pQuasiSyntax
  Parsel.whitespaces
  Parsel.string "...*)"
  Parsel.whitespaces
  pure (QuasiSyntaxSome stx)