{-# LANGUAGE TemplateHaskellQuotes #-}

module Opal.Expand.Syntax.TH (
  opal'procedure,
) where

import Data.Text qualified as Text

import Language.Haskell.TH (Exp, Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter)
import Language.Haskell.TH.Quote qualified as TH.Quote
import Language.Haskell.TH.Syntax qualified as TH.Syntax

import Text.Parsel qualified as Parsel

--------------------------------------------------------------------------------

import Opal.Core (Expr, SExp (..))
import Opal.Core.Datum (Datum)
import Opal.Core.Datum qualified as Datum

import Opal.Expand.Syntax (Syntax)

import Opal.Parse (evalParseExpr)
import Opal.Parse qualified as Opal

import Opal.Read qualified as Read
import qualified Opal.Print as Print

--------------------------------------------------------------------------------

opal'procedure :: QuasiQuoter
opal'procedure =
  TH.Quote.QuasiQuoter
    { TH.Quote.quoteExp = qqOpalMacro
    , TH.Quote.quotePat = const (throwQuoteContextError "pattern")
    , TH.Quote.quoteType = const (throwQuoteContextError "type")
    , TH.Quote.quoteDec = const (throwQuoteContextError "declaration")
    }
  where
    throwQuoteContextError :: String -> Q a
    throwQuoteContextError ctx = do
      TH.reportError (shows 'opal'procedure " used in " ++ ctx ++ " context")
      fail (shows 'opal'procedure ": can not splice in an expression context.")

qqOpalMacro :: String -> Q Exp
qqOpalMacro src = qqParseOpalMacro src >>= TH.Syntax.lift

qqParseOpalMacro :: String -> Q Datum
qqParseOpalMacro src = do
  stx <- qqReadOpalMacro src
  case evalParseExpr stx of
    Left exn -> failParseErrorQ exn
    Right sexp@SExpVar {} -> failResultErrorQ sexp
    Right sexp@SExpApp {} -> failResultErrorQ sexp
    Right sexp@SExpIf {} -> failResultErrorQ sexp
    Right sexp@SExpLet {} -> failResultErrorQ sexp
    Right sexp@(SExpVal datum) 
      | Datum.isProcDatum datum -> pure datum
      | otherwise -> failResultErrorQ sexp
  where
    failParseErrorQ :: Opal.ParseError -> Q a
    failParseErrorQ exn = do
      TH.reportError (show exn)
      fail "error encountered, could not parse syntax."

    failResultErrorQ :: Expr -> Q a 
    failResultErrorQ sexp = do 
      let str = Text.unpack (Print.pprExpr sexp)
      fail ("parsed expression did not produce a macro (not a procedure):\n " ++ str)

qqReadOpalMacro :: String -> Q Syntax
qqReadOpalMacro src = do
  case Read.runRead (Text.pack src) of
    Left exn -> failReadErrorQ exn
    Right stx -> pure stx
  where
    failReadErrorQ :: Parsel.ParseError -> Q a
    failReadErrorQ exn = do
      TH.reportError (show exn)
      fail "error encountered, could not read syntax."