module Opal.Run.Command
  ( -- * TODO
    Command (CmdEval, CmdExpand, CmdParse, CmdRead),
  )
where

import Data.Kind (Type)

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------

data Command :: Type where 
  CmdEval :: [FilePath] -> Command
  CmdExpand :: [FilePath] -> Command
  CmdParse :: [FilePath] -> Command
  CmdRead :: [FilePath] -> Command