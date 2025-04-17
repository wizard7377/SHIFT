{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Solve where

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
import Extra.Tuple
import Rift.Core.Instances ()

import Rift.Core.Unify.Types

-- import Rift.Core ()
import Rift.Core.Base
import Rift.Core.Unify.Unify

-- TODO

-- | Attempt to simplify the unification as much as possible
solve :: (Atomic atom) => UTree (Term atom) -> UTree (Term atom)
solve uni = case solveStep <| ("uni" <?> uni) of
  uni' | uni == uni' -> uni'
  uni' | otherwise -> solve uni'

-- | Remove all binds of @x@ to @y@ where @x@ is equal to @y@ and neither are free
reflex :: (Atomic atom) => ULeaf (Term atom) -> UTree (Term atom)
reflex (uni, state) =
  let
    lvarp = \(x, y) -> x `elem` state ^. freeLeft
    rvarp = \(x, y) -> y `elem` state ^. freeRight
    (free, notFree) = partition (\b -> lvarp b %% rvarp b) (uni ^. unknown)
    (reds, unreds) = partition (\(x, y) -> x /= y) notFree
   in
    if not $ null reds then cabsurd else pure (set unknown [] uni, state)

-- TODO needs some work
solveStep :: (Atomic atom) => ULeaf (Term atom) -> UTree (Term atom)
solveStep (uni, state) =
  if (uni ^. unknown) /= []
    then do
      let dvarp = \(x, y) -> x `elem` state ^. freeLeft && y `elem` state ^. freeRight
      let lvarp = \(x, y) -> x `elem` state ^. freeLeft
      let rvarp = \(x, y) -> y `elem` state ^. freeRight
      -- FIXME
      case "bounds" <?> split4 lvarp rvarp (uni ^. unknown) of
        (simple, [], [], rest) -> (reflex (simple, state)) >> pure (rest, state)
        (simple, boundLeft, boundRight, rest) ->
          let
            state1 = snd <$> reflex (simple, state)
            lowerLeft = mapToF boundLeft
            lowerRight = mapToFR boundRight
            rest' = (map (bimap lowerLeft lowerRight) rest)
            newLeaf = (,) (simpleResult rest') <$> state1
            _ = "newLeaf" <?> newLeaf
           in
            first ((bindingLeft .~ boundLeft) . (bindingRight .~ boundRight)) <$> (solveStep <| newLeaf)
    else simple (uni, state)

-- let ovar1 = first (mapToF lvar) <$> lvar
-- let ovar2 = second (mapToF rvar) <$> rvar
-- let state0 = state & freeLeft %~ (\\ (mapToF ovar2 <$> fst <$> lvar))
-- (ovar2, state)

require :: (Atomic atom) => [Term atom] -> UTree (Term atom) -> Choice [Term atom]
require reqs = cfilterMap (require' reqs)
require' :: (Atomic atom) => [Term atom] -> ULeaf (Term atom) -> Maybe [Term atom]
require' reqs leaf@(uni, state) =
  let
    -- \|Get all the relavent keys
    keys = map fst $ filter (\(x, y) -> y `elem` reqs) uni
    -- \|Then all the relavent values
    focused = filter (\(x, y) -> x `elem` reqs) uni
    allFree = any (\(_, y) -> y `notElem` reqs) uni
    oneToOne = minject focused -- TODO maybe
   in
    if allFree && oneToOne then Just keys else Nothing
