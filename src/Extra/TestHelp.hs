module Extra.TestHelp where

import Data.Text qualified as T

makeTestList ::
  -- | The function to apply
  (T.Text -> a) ->
  -- | Where to split
  String ->
  -- | Input
  String ->
  -- | Result
  [a]
makeTestList f v s = if s == "" then [] else f <$> T.splitOn (T.pack v) (T.pack s)
