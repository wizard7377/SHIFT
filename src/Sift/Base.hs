module Sift.Base where

import Data.Kind (Type)
import Rift
import Rift qualified

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

{- | The solver atomic type.
 Note that this is an _atom_, and not a _term_, as to allow for unification
 That is, we have @Term (SAtom atom)@
 However, `SAtom` is paramaterized over an "inner attom"
-}
data SAtom atom where
  Simple :: atom -> SAtom atom
  He :: Term atom -> SAtom atom
  deriving (Show, Eq, Ord)

type STerm atom = Term (SAtom atom)

{- | Qualified terms, that is, terms that have some _He_'s in them
 The first part is the term, the second part the frees
-}
type QTerm tok = (STerm tok, [STerm tok])

toSTerm :: Term atom -> Term (SAtom atom)
toSTerm = fmap Simple

-- TODO makes this work without incompleteness
fromSAtom :: SAtom atom -> atom
fromSAtom term = case term of
  Simple val -> val
  _ -> _

fromSTerm :: STerm atom -> Term atom
fromSTerm term = fromSAtom <$> term

qbinds :: QTerm tok -> [STerm tok]
qbinds = snd
qterm :: QTerm tok -> STerm tok
qterm = fst
