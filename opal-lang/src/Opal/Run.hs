module Opal.Run
  ( runTestFile,
    testExpand,
  )
where

--------------------------------------------------------------------------------

import Data.IO.FileBuffer (openFileBufferIO)

import Opal.Expand (evalExpM, runStxExpand)
import Opal.Expr (Expr)
import Opal.Lexer (runLexer, lexStx)

--------------------------------------------------------------------------------

runTestFile :: IO Expr
runTestFile = testExpand "opal-lang/test-files/test.opal"

testExpand :: FilePath -> IO Expr
testExpand filepath = do
  buf <- openFileBufferIO filepath
  stx <- case runLexer buf lexStx of
    Left exn -> error ("lexer error: " ++ show exn)
    Right stx -> do 
      print stx
      pure stx
  case evalExpM (runStxExpand stx) of
    Left exn -> error ("expand error: " ++ show exn)
    Right expr -> pure expr