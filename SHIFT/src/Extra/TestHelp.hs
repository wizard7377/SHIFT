module Extra.TestHelp where

import Data.List (delete)
import Data.Text qualified as T

cleanUp :: String -> String
cleanUp ('\\' : x : rest) = '!' : cleanUp rest
cleanUp (x : y) = x : cleanUp y
cleanUp [x] = [x]
cleanUp [] = []
