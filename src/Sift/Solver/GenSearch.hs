{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (makeLenses, over, set, view, (%=), (&), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (intersect, singleton, subsequences)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Basics

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Extra.Choice
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift
import Rift (Atomic)
import Sift (LogicResult (..))
import Sift.Base (LogicEnv, SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift
import Sift.Types.Generate

-- import Sift.Types.Unify

type LMGen sen atom a = LM (SearchState sen atom) a

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState sen atom = SearchState
  { _sentences :: [sen atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  , _gen :: Generator
  }

makeLenses ''SearchState
instance EnterState SearchState Term where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      , _gen = Generator 0
      }

genSolve :: (Atomic atom) => LMGen sen atom (LogicResult ())
genSolve = _

he :: (Atomic atom) => LMGen sen atom (VTerm atom)
he = do
  heGenV <- gets _gen
  let (vatom, heGenV') = heGen heGenV
  heGenV' <- gen .= heGenV'
  return (Atom vatom)
