{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init
import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (singleton, subsequences)
import Data.Maybe (catMaybes, isJust, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Basics

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))
import Extra.List (forEach, subParts)
import Rift
import Rift (Atomic)
import Sift (LogicResult (..))
import Sift.Base (LogicEnv, SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- import Sift.Types.Unify

type LMGen sen atom a = (Atomic atom) => LM (SearchState sen atom) a

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState sen atom = SearchState
  { _sentences :: [sen atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  }

instance EnterState SearchState Term where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      }

genSolve :: LMGen sen atom (LogicResult ())
genSolve = _

yudGen :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudGen term = do
  let (t0, f0) = intros term
  let rebinds = subParts f0
  let terms = fmap (\(inners, outers) -> (unintros (t0, inners), outers)) rebinds
  _

yudGen' :: (Atomic atom) => QTerm atom -> Term atom
yudGen' (term, f0) = unintros ((Rule term Yud), f0)
