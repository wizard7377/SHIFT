module Sift.Base where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Kind (Type)
import Rift
import Rift qualified

-- | The result of a logic solve
data LogicResult
  = -- | Solved
    Solved
  | -- | Proven unsolvable (only possible in certain solvers)
    Unsolved
  | -- | Stopped, most likely due to overrunning @_depth@
    Stopped
  deriving (Show, Eq)

instance Semigroup LogicResult where
  Solved <> x = x
  Unsolved <> _ = Unsolved
  Stopped <> _ = Stopped
