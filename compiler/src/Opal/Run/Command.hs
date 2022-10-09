module Opal.Run.Command
  ( -- * TODO
    Command (CmdEval, CmdParse, CmdRead),
  )
where

import Data.Kind (Type)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data Command :: Type where 
  CmdEval :: [FilePath] -> Command 
  CmdParse :: [FilePath] -> Command
  CmdRead :: [FilePath] -> Command