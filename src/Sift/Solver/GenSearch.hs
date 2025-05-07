-- TODO IMPLEMENT lineararity
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE UndecidableInstances #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (Lens', lens, makeLenses, over, set, view, (%=), (&), (+=), (.=), (.~), (^.), _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity (..))

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Control.Monad.RWS (MonadReader (..))
import Control.Monad.State
import Data.Foldable (Foldable (..), asum)
import Data.List (intersect, singleton, subsequences, (\\))
import Data.List.Extra (nub)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra
import Extra.Basics
import Extra.Choice hiding (resolve)
import Extra.Choice qualified
import Extra.Error (Error)
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift
import Rift (Atomic)
import Sift.Base (LogicEnv, LogicResult (..), SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Base qualified as Base
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- import Sift.Types.Unify

-- TODO temporary
maxDepth = 8
type LMGen atom a = LM (SearchState atom) a

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState atom = SearchState
  { _sentences :: [Term' atom]
  , _scratch :: [FTerm atom]
  -- ^ All proven statements
  , _depth :: Int
  -- ^ Current depth to end
  , _vars :: Int
  }

depth :: Lens' (SearchState atom) Int
depth = lens _depth (\s d -> s{_depth = d})
scratch :: Lens' (SearchState atom) [FTerm atom]
scratch = lens _scratch (\s d -> s{_scratch = d})
sentences :: Lens' (SearchState atom) [Term' atom]
sentences = lens _sentences (\s d -> s{_sentences = d})
vars :: Lens' (SearchState atom) Int
vars = lens _vars (\s d -> s{_vars = d})
deriving instance (Show atom) => Show (SearchState atom)

instance EnterState SearchState where
  enterState :: LogicEnv -> [Term' atom] -> [Term' atom] -> SearchState atom
  enterState env sens scratch =
    SearchState
      { _sentences = sens
      , _scratch = simpleF <$> (scratch <> sens)
      , _depth = 0
      , _vars = 0
      }

showTerms :: (Show atom) => [FTerm atom] -> String
showTerms sens = concatMap (\s -> show s) sens
showSens :: (Show atom) => SearchState atom -> String
showSens = showTerms . _scratch
genSearch ::
  (Atomic atom) =>
  Term' atom ->
  LMGen atom LogicResult
genSearch = genSearch'
genSearch' ::
  (Atomic atom) =>
  Term' atom ->
  LMGen atom LogicResult
genSearch' goal = do
  depths <- gets _depth
  "Depth" ?@>> depths
  scratchold <- gets _scratch
  "Scratch" ?@>> scratchold
  let results = concat $ mem <$> scratchold <*> scratchold
  "Results" ?@>> results
  let newresults = results
  "New results" ?@>> newresults
  let newscratch = scratchold <> newresults
  "New scratch" ?>> nub newscratch
  let newdepth = depths + 1
  _ <- depth .= newdepth
  _ <- scratch .= nub newscratch
  if resolved newscratch goal
    then "Solved" ?> return Solved
    else (if newdepth < maxDepth then "Go again" ?> genSearch' goal else "Stopped" ?> return Stopped)

resolved ::
  (Atomic atom) =>
  [FTerm atom] ->
  Term' atom ->
  Bool
resolved vals goal =
  any
    ( \(FTerm t v) ->
        let
          bindc = "Bindc" <?@> generate goal t
          env = "env" <?@> ((\x -> initEnv x [] v) <$> bindc)
          unifyResultChoice = "Result" <?@> unify <| env
         in
          (not (null unifyResultChoice))
    )
    vals

-- | The basic mem rule
mem ::
  (Atomic atom) =>
  -- | The transform, top value $?x A B$
  FTerm atom ->
  -- | The transformed, bottom value, $B$
  FTerm atom ->
  [FTerm atom]
mem top@(FTerm (Lamed var upTo upFrom) freeUp) bottom@(FTerm down freeDown) =
  let
    uni = generate upFrom down
    env = (initEnv <$> uni) <*> pure (var : freeUp) <*> pure freeDown
    res =
      ( \ures ->
          let
            mapping = mapToF $ ures ^. binds
            newvars = ures ^. varsUp
           in
            FTerm (recurseSome mapping upTo) newvars
      )
        <$> env
   in
    res
mem _ _ = []
