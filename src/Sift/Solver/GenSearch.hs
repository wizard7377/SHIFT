{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (over, _1, _2)
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
  }

instance EnterState SearchState Term where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      }

genSolve :: (Atomic atom) => LMGen sen atom (LogicResult ())
genSolve = _

memFill :: (Atomic atom) => Term atom -> Term atom -> LMGen sen atom [Term atom]
memFill termA termB = do
  let termA1 = intros termA
  let termB1 = intros termB
  return $ case (termA1 ^. root, termB1 ^. root) of
    (Rule gA pA, Rule gB pB) ->
      let pA1 = intros pA
          gB1 = intros gB
          uni = unify (gB1 ^. root) (pA1 ^. root)
          newTerm = Rule gA pB
          possible =
            cfilterMap
              ( \choice ->
                  let
                    -- (Not free to not free, free to not free, not free to free, free to free)
                    (basic, freeing, incomplete, binding) = split4 (\e -> fst e `elem` gB1 ^. free) (\e -> snd e `elem` pA1 ^. free) choice
                    -- If anything constant has tried to become free
                    goodAchieve = null incomplete
                    isOnto = msuject binding
                    -- A variable does not both bind to a free and a not free
                    isSingle = intersect (getKeys freeing) (getKeys binding) == []
                   in
                    if goodAchieve && isSingle && isOnto then Just basic else Nothing
              )
              uni
          vtree = uTree possible (termA1 ^. free) (termB1 ^. free)
          stree = Rift.solve vtree
          prepReq = cfilterMap $ \(binds, state) -> if null state then Just binds else Nothing
          prepReqV = prepReq stree
          -- TODO
          prepMap = mapToF <$> prepReqV
          -- TODO
          prep = prepMap <*> pure newTerm
       in resolve prep
    _ -> []

-- | ?f(x : y) -> (?f(x) : ?f(y))
lamedSplit :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
lamedSplit = lamedSplit' . intros

lamedSplit' :: (Atomic atom) => QTerm atom -> LMGen sen atom [Term atom]
lamedSplit' term = return $ case term ^. root of
  (Rule to from) -> [Rule (addTo to (term ^. free)) (addTo from (term ^. free))]
  _ -> []

-- | x -> (x : *)
yudGen :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudGen term = do
  let qterm = intros term
  let parts = partApply (\conq -> singleton $ Rule (unintros conq) Yud) $ mkQTerm (qterm ^. root) (qterm ^. free)
  return $ unintros <$> parts

-- | (x : *) -> x
yudRed :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudRed = yudRed' . intros

yudRed' :: (Atomic atom) => QTerm atom -> LMGen sen atom [Term atom]
yudRed' term = return $ case term ^. root of
  Rule val Yud -> [unintros $ mkQTerm val (term ^. free)]
  _ -> []
yudGen' :: (Atomic atom) => QTerm atom -> Term atom
yudGen' term = unintros $ mkQTerm (Rule (term ^. root) Yud) (term ^. free)
