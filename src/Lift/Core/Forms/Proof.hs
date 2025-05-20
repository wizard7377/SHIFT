module Lift.Core.Forms.Proof where

import Data.Text qualified as T
import Extra

data Proof t
  = Proof
  { _term :: t
  , _name :: T.Text
  , _desc :: T.Text
  }
  deriving (Eq, Show, Ord, Data, Generic)
