module Opal.Run
  ( runTestFile,
    testExpand,
  )
where

--------------------------------------------------------------------------------

import Data.IO.FileBuffer (openFileBufferIO)

import Opal.Expand.Monad ( evalExpand )
import Opal.Expr (Expr)
import Data.Parse (runBufferParse)
import Opal.Reader (rSyntax)
import Opal.Expand.Monad (ExpError)
import Opal.Expand (expStx)
import qualified Opal.Expand.Syntax as Syntax
import Control.Monad.Reader (asks)
import Opal.Expand.Context (Context(ctxPhase, ctxCoreScope))

--------------------------------------------------------------------------------

runTestFile :: IO (Either ExpError Expr)
runTestFile = testExpand "opal-lang/test-files/test.opal"

testExpand :: FilePath -> IO (Either ExpError Expr)
testExpand filepath = do
  buf <- openFileBufferIO filepath
  case runBufferParse buf "test.opal" rSyntax of
    Left exn -> error ("lexer error: " ++ show exn)
    Right stx -> pure $ evalExpand do 
      phase <- asks ctxPhase
      coreScope <- asks ctxCoreScope 
      expStx (Syntax.scope phase coreScope stx)

      
  -- case evalExpand (runStxExpand stx) of
  --   Left exn -> error ("expand error: " ++ show exn)
  --   Right expr -> pure expr