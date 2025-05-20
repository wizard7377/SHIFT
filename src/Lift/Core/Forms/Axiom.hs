module Lift.Core.Forms.Axiom where

import Data.Text qualified as T
import Extra

data Statement t
  = Nameless t
  | Statement
      { _term :: t
      , _name :: T.Text
      , _desc :: T.Text
      }
  deriving (Eq, Show, Ord, Data, Generic)
