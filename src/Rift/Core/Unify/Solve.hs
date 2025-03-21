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
solve :: (Atomic atom) => UTree atom -> UTree atom
solve uni = case cfilterMap id $ solveStep <$> ("uni" <?> uni) of
  uni' | uni == uni' -> uni'
  uni' | otherwise -> solve uni'

-- | Remove all binds of @x@ to @y@ where @x@ is equal to @y@ and neither are free
reflex :: (Atomic atom) => ULeaf atom -> ULeaf atom
reflex (uni, state) =
  let
    lvarp = \(x, y) -> x `elem` state ^. freeLeft
    rvarp = \(x, y) -> y `elem` state ^. freeRight
    (free, notFree) = partition (\b -> lvarp b %% rvarp b) uni
    reds = filter (\(x, y) -> x /= y) notFree
   in
    (free <> reds, state) -- TODO Make these change state

-- TODO needs some work
solveStep :: (Atomic atom) => ULeaf atom -> Maybe (ULeaf atom)
solveStep (uni, state) =
  if uni /= []
    then
      ( do
          let dvarp = \(x, y) -> x `elem` state ^. freeLeft && y `elem` state ^. freeRight
          let lvarp = \(x, y) -> x `elem` state ^. freeLeft
          let rvarp = \(x, y) -> y `elem` state ^. freeRight
          -- FIXME
          let (simple, boundLeft, boundRight, lifted) = "bounds" <?> split4 lvarp rvarp uni
          let newState' = over freeLeft (\\ (fst <$> boundLeft)) state
          let newState@(UState newLeft newRight) = "newState" <?> over freeRight (\\ (snd <$> boundLeft)) state
          let mapLeft = mapToF boundLeft
          let mapRight = mapToF $ flipflop boundRight
          let qBinds = bimap mapLeft mapRight <$> lifted
          (resBinds, resState) <- solveStep (qBinds, newState)
          -- TODO
          "result" <?> case reflex (simple, newState) of
            ([], _) -> Just (boundLeft <> boundRight <> resBinds, resState)
            _ -> Nothing
      )
    else Just (uni, state)

-- let ovar1 = first (mapToF lvar) <$> lvar
-- let ovar2 = second (mapToF rvar) <$> rvar
-- let state0 = state & freeLeft %~ (\\ (mapToF ovar2 <$> fst <$> lvar))
-- (ovar2, state)

require :: (Atomic atom) => [Term atom] -> UTree atom -> Choice [Term atom]
require reqs = cfilterMap (require' reqs)
require' :: (Atomic atom) => [Term atom] -> ULeaf atom -> Maybe [Term atom]
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
