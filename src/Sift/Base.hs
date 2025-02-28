module Sift.Base where

import Data.Kind (Type)
import Rift qualified
import Rift.Base (Term (..))
import Rift.Instances ()

-- | The result of a logic solve
data LogicResult e
  = -- | Solved
    Solved
  | -- | Proven unsolvable (only possible in certain solvers)
    Unsolved
  | -- | Stopped, most likely due to overrunning @_depth@
    Stopped
  | -- | Failed with an error
    Failed e
  deriving (Show, Eq)

-- | The enviroment of a logical solve
data LogicEnv = LogicEnv
  { _depth :: Maybe Int
  -- ^ The depth it is allowed to go to
  }

defaultEnv :: LogicEnv
defaultEnv =
  LogicEnv
    { _depth = Just 10
    }

data SAtom atom where
  Token :: atom -> SAtom atom
  Leave :: STerm atom -> SAtom atom
  He :: STerm atom -> SAtom atom
  Wild :: SAtom atom
type STerm (tok :: Type) = Term (SAtom tok)
type QTerm tok = (STerm tok, [STerm tok])
toSTerm :: Term atom -> STerm atom
toSTerm = fmap Token
