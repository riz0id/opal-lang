{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Syntax.TH
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
module Opal.Syntax.TH
  ( syntax,
  )
where

import Control.Monad ((>=>))

import Language.Haskell.TH (Pat (..), Q, Exp (..))
import Language.Haskell.TH.Quote (QuasiQuoter (..))

import Opal.Quasi (qexpToSyntaxE, qexpToSyntaxP)
import Opal.Quasi.Reader (runQuasiReader)

-- Quasi-Quotation -------------------------------------------------------------

-- | TODO: docs
--
-- @since 1.0.0
syntax :: QuasiQuoter
syntax =
  QuasiQuoter
    { quoteExp  = readSyntaxExpression
    , quotePat  = readSyntaxPattern
    , quoteType = undefined -- FIXME: unimplemented
    , quoteDec  = undefined -- FIXME: unimplemented
    }

-- | TODO: docs
--
-- @since 1.0.0
readSyntaxExpression :: String -> Q Exp
readSyntaxExpression = runQuasiReader >=> qexpToSyntaxE

-- | TODO: docs
--
-- @since 1.0.0
readSyntaxPattern :: String -> Q Pat
readSyntaxPattern = runQuasiReader >=> qexpToSyntaxP
