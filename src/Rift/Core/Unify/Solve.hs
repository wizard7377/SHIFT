{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Solve where

import Control.Arrow
import Control.Lens (over, set, view, (%~), (&), (.~), (^.))
import Control.Lens qualified as Lens
import Control.Monad hiding (filter)
import Control.Monad.State
import Data.List (find, partition, (\\))
import Data.Text qualified as T
import Debug.Trace
import Extra.Basics hiding (Mapping)
import Extra.Choice hiding (solve)
import Extra.Map
import Rift.Core.Instances ()

-- import Rift.Core ()
import Rift.Core.Base
import Rift.Core.Unify.Unify

type QUnify a = ([Binding (Term a)], UState a)

type UM a b = State (UState a) b

-- | Attempt to simplify the unification as much as possible
solve :: (Atomic atom) => UTree atom -> UTree atom
solve uni = case solveStep <$> uni of
  uni' | uni == uni' -> uni'
  uni' | otherwise -> solve uni'

-- | Remove all binds of @x@ to @y@ where @x@ is equal to @y@ and neither are free
reflex :: (Atomic atom) => ULeaf atom -> ULeaf atom
reflex (uni, state) =
  let
    lvarp = \(x, y) -> x `elem` state ^. freeLeft
    rvarp = \(x, y) -> y `elem` state ^. freeRight
    (used, notUsed) = partition (\(x, y) -> (lvarp (x, y) || rvarp (x, y))) uni
    reds = filter (\(x, y) -> x /= y) notUsed
   in
    (used <> reds, state)

-- TODO needs some work
solveStep :: (Atomic atom) => ULeaf atom -> ULeaf atom
solveStep (uni, state) =
  let
    dvarp = \(x, y) -> x `elem` state ^. freeLeft && y `elem` state ^. freeRight
    lvarp = \(x, y) -> x `elem` state ^. freeLeft
    rvarp = \(x, y) -> y `elem` state ^. freeRight
    ovar = filter (\v -> not (lvarp v %% rvarp v)) uni
    lvar = filter lvarp uni
    rvar = filter rvarp uni
    ovar1 = first (mapToF lvar) <$> ovar
    ovar2 = second (mapToF rvar) <$> ovar1
    state0 = state & freeLeft %~ (\\ (mapToF ovar2 <$> fst <$> lvar))
   in
    (ovar2, state)
