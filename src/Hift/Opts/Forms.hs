module Hift.Opts.Forms where

import Hift.Opts.Types
import Options.Applicative

mainOpts :: ParserInfo ProgOpts
mainOpts =
  info
    (progOpts <**> helper)
    ( fullDesc
        <> progDesc
          " \
          \ (SHIFT) is a solver for Flat Types. It features and can be extended by a number of langauges for input and solver for output. \
          \ It can be used to typeset, prove, and create formal mathmatical proofs, as well as write small programs. \
          \ "
        <> header "The Solver for Higher Intutionisistic Flat Theorms (SHIFT)"
    )

progOpts :: Parser ProgOpts
progOpts =
  ProgOpts
    <$> argument str (metavar "INPUT")
    <*> strOption
      ( long "language"
          <> short 'l'
          <> metavar "LANG"
          <> value "mift"
          <> help "The language to parse the input with"
      )
    <*> hsubparser
      (command "repl" (info (Repl <$> replOptions) (progDesc "Run the REPL")))

replOptions :: Parser ReplOptions
replOptions = pure ReplOptions
