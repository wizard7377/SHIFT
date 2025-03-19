module Extra.Error where

import Data.Text (Text)

{- | The type of low level errors used throughout SHIFT
 Note that it should be _highly prefered_ to throw one of these instead of panicking
-}
data Error
  = MatchError Text
  | Other Text
  deriving (Eq, Show, Ord)
