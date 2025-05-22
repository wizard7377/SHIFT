module Hift.Hift (runMain) where

import Hift.Opts.Forms
import Hift.Opts.Types
import Options.Applicative (execParser)

runMain :: IO ()
runMain = do
  opts <- execParser mainOpts
  runHift opts

runHift :: ProgOpts -> IO ()
runHift opts = do
  putStrLn $ show opts
