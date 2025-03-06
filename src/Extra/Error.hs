module Extra.Error where

import Data.Text (Text)

data Error
  = MatchError Text
  | Other Text
