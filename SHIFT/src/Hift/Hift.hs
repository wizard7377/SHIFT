module Hift.Hift (runMain) where

import Data.Default
import Hift.Opts.Forms
import Hift.Opts.Types
import Hift.Toplevel
import Options.Applicative (execParser)

runMain :: IO ()
runMain = do
  opts <- execParser mainOpts
  runHift opts

runHift :: (Default t) => ProgOpts -> IO t
runHift opts = do
  runHiftM topLevel opts def
