module Extra.Error where

import Control.Exception (Exception)
import Data.Text (Text)
import Data.Text qualified as T

{- | The type of low level errors used throughout SHIFT
 Note that it should be _highly prefered_ to throw one of these instead of panicking
-}
data Error
  = MatchError Text
  | Other Text
  | OutOfBounds
  | SymbolNotFound
  | MonadFailing T.Text
  | TODO
  deriving (Eq, Show, Ord)

instance Exception Error
