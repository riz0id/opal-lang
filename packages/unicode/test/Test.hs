module Main (main) where

import Test.Data.Unicode qualified
import Test.Tasty (defaultMain)

main :: IO ()
main = defaultMain Test.Data.Unicode.testTree
