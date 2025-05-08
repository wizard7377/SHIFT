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

-- | The enviroment of a logical solve
data LogicEnv = LogicEnv
  { _depth :: Int
  -- ^ The depth it is allowed to go to
  }

defaultEnv :: LogicEnv
defaultEnv =
  LogicEnv
    { _depth = 24
    }

{- | The solver atomic type.
 Note that this is an _atom_, and not a _term_, as to allow for unification
 That is, we have @Term' (SAtom atom)@
 However, `SAtom` is paramaterized over an "inner attom"
-}
data SAtom atom where
  Simple :: atom -> SAtom atom
  He ::Term atom -> SAtom atom
  deriving (Show, Eq, Ord)

type STerm atom =Term (SAtom atom)
