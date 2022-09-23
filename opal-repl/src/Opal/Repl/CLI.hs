module Opal.Repl.CLI
  ( -- * TODO
    InputCLI (EvalCLI, CmdCLI),

    -- * TODO
    CommandCLI
      ( ExpandCmd,
        LexCmd,
        HelpCmd,
        LoadCmd,
        ReloadCmd,
        QuitCmd
      ),

    -- * TODO
    ParseError (NoInputError, CommandError, SyntaxError),
  )
where

import Opal.Expand.Syntax (Syntax)
import Opal.Lexer qualified as Lexer

--------------------------------------------------------------------------------

data InputCLI
  = EvalCLI Syntax
  | CmdCLI CommandCLI
  deriving (Eq, Ord, Show)

data CommandCLI
  = ExpandCmd
  | LexCmd
  | HelpCmd
  | LoadCmd
  | ReloadCmd
  | QuitCmd
  deriving (Enum, Eq, Ord, Show)

data ParseError
  = NoInputError
  | CommandError String
  | SyntaxError Lexer.Error
  deriving (Eq, Ord, Show)
