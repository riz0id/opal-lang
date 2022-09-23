module Opal.Repl.Parse
  ( -- * TODO
    InputCLI (EvalCLI, CmdCLI),

    -- * TODO
    CommandCLI (HelpCmd, LoadCmd, QuitCmd),

    -- * TODO
    ParseError (NoInputError, CommandError, SyntaxError),

    -- * TODO
    parseCLI,
  )
where

import Opal.Lexer qualified as Lexer

--------------------------------------------------------------------------------

import Opal.Repl.CLI (CommandCLI, InputCLI, ParseError)
import Opal.Repl.CLI qualified as CLI

--------------------------------------------------------------------------------

parseCLI :: String -> Either ParseError InputCLI
parseCLI input
  | null input = Left CLI.NoInputError
  | otherwise =
      if ':' == head input
        then fmap CLI.CmdCLI (pCmd (tail input))
        else case Lexer.runStringLexer input Lexer.lexStx of
          Left err -> Left (CLI.SyntaxError err)
          Right rx -> Right (CLI.EvalCLI rx)
  where
    pCmd :: String -> Either ParseError CommandCLI
    pCmd "expand" = Right CLI.ExpandCmd
    pCmd "lex" = Right CLI.LexCmd
    pCmd "?" = Right CLI.HelpCmd
    pCmd "l" = Right CLI.LoadCmd
    pCmd "r" = Right CLI.ReloadCmd
    pCmd "q" = Right CLI.QuitCmd
    pCmd cmd = Left (CLI.CommandError cmd)
