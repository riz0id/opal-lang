{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes      #-}

{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.NonEmpty.TH
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
--
-- @since 1.0.0
module Opal.Common.NonEmpty.TH
  ( -- * QuasiQuoters
    nonEmptyString
    -- ** Parse
  , runParseNonEmptyStringE
  , parseNonEmptyStringE
    -- * Template Haskell
  , nonEmptyE
  , nonEmptyE'
  , liftNonEmpty
  , liftNonEmptyWith
  )
where

import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void)

import GHC.Exts (Char (..))

import Language.Haskell.TH (Exp (..), Lit (..), Loc (..), Q)
import Language.Haskell.TH qualified as TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift (..))
import Language.Haskell.TH.Syntax qualified as TH

import Text.Megaparsec
import Text.Megaparsec.Char (space)

-- QuasiQuoters ----------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
nonEmptyString :: QuasiQuoter
nonEmptyString =
  QuasiQuoter
    { quoteExp  = runParseNonEmptyStringE
    , quotePat  = undefined -- FIXME: unimplemented
    , quoteType = undefined -- FIXME: unimplemented
    , quoteDec  = undefined -- FIXME: unimplemented
    }

-- QuasiQuoters - Parse --------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
runParseNonEmptyStringE :: String -> Q Exp
runParseNonEmptyStringE input = do
  filepath <- fmap loc_filename TH.location
  case parse parseNonEmptyStringE filepath input of
    Left  exn -> fail (errorBundlePretty exn)
    Right xs  -> pure (liftNonEmptyStringE xs)
  where
    liftNonEmptyStringE :: NonEmpty Char -> Exp
    liftNonEmptyStringE (chr :| chrs) = do
      let chrE  = ConE 'C# `AppE` LitE (CharPrimL chr)
          chrsE = LitE (StringL chrs)
       in ConE '(:|) `AppE` chrE `AppE` chrsE


-- | TODO: docs
--
-- @since 1.0.0
parseNonEmptyStringE :: Parsec Void String (NonEmpty Char)
parseNonEmptyStringE = do
  void (space *> single '"')
  str <- liftA2 (:|) strChar (many (try strChar) <|> pure [])
  void (single '"' *> space)
  str <$ eof
  where
    strChar :: Parsec Void String Char
    strChar = satisfy (/= '"')

-- Template Haskell ------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
nonEmptyE :: NonEmpty (Q Exp) -> Q Exp
nonEmptyE = fmap nonEmptyE' . sequence

-- | TODO: docs
--
-- @since 1.0.0
nonEmptyE' :: NonEmpty Exp -> Exp
nonEmptyE' (ex :| rest) = case NonEmpty.nonEmpty rest of
  Nothing -> makeNonEmptyE (ConE '[])
  Just xs -> makeNonEmptyE (nonEmptyE' xs)
  where
    makeNonEmptyE :: Exp -> Exp
    makeNonEmptyE exs = ConE '(:|) `AppE` ex `AppE` exs

-- | TODO: docs
--
-- @since 1.0.0
liftNonEmpty :: Lift a => NonEmpty a -> Q Exp
liftNonEmpty = liftNonEmptyWith TH.lift

-- | TODO: docs
--
-- @since 1.0.0
liftNonEmptyWith :: (a -> Q Exp) -> NonEmpty a -> Q Exp
liftNonEmptyWith f xs = nonEmptyE (fmap f xs)