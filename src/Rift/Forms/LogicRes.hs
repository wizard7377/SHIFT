module Rift.Forms.LogicRes where

-- | The result of a logic solve
data LogicResult p
  = -- | Solved
    Solved p
  | -- | Proven unsolvable (only possible in certain solvers)
    Unsolved
  | -- | Stopped, most likely due to overrunning @_depth@
    Stopped
  deriving (Show, Eq)

instance Semigroup (LogicResult p) where
  Solved p <> x = x
  Unsolved <> _ = Unsolved
  Stopped <> _ = Stopped
