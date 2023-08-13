{-# OPTIONS_HADDOCK show-extensions #-}

-- |
-- Module      :  Test
-- Copyright   :  (c) Jacob Leach, 2023
-- License     :  ISC, see LICENSE
--
-- Maintainer  :  jacobleach@protonmail.com
-- Stability   :  stable
-- Portability :  non-portable (GHC extensions)
--
-- TODO: docs
module Main (main) where

import Test.Opal qualified
import Test.Tasty (defaultMain)

--------------------------------------------------------------------------------

main :: IO ()
main = defaultMain Test.Opal.testTree
