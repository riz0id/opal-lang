{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Opal.Common.Lens
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
module Opal.Common.Lens
  ( -- * Lenses
    opalLensRules
  , opalLensNamer
  , defineLenses
  )
where

import Control.Lens (FieldNamer, LensRules, DefName (..), lensRules, makeLensesWith)
import Control.Lens.Internal.FieldTH (LensRules (..))

import Language.Haskell.TH (Dec (..), Name, Q)
import Language.Haskell.TH qualified as TH
import Data.Char (toUpper)

-- Lenses ----------------------------------------------------------------------

-- TODO: docs
--
-- @since 1.0.0
opalLensRules :: LensRules
opalLensRules = lensRules { _fieldToDef = opalLensNamer }

-- TODO: docs
--
-- @since 1.0.0
opalLensNamer :: FieldNamer
opalLensNamer _ _ fieldNm = [TopName (TH.mkName (snakeCaseNamer (TH.nameBase fieldNm)))]
  where
    snakeCaseNamer :: String -> String
    snakeCaseNamer ""              = ""
    snakeCaseNamer [c]             = [c]
    snakeCaseNamer ('_' : c  : cs) = snakeCaseNamer (toUpper c : cs)
    snakeCaseNamer (c1  : c2 : cs) = c1 : snakeCaseNamer (c2 : cs)

-- | TODO: docs
--
-- @since 1.0.0
defineLenses :: Name -> Q [Dec]
defineLenses = makeLensesWith opalLensRules