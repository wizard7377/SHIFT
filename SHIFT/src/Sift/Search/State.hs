{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Search.State
License     : BSD-2-Clause
Maintainer  : Asher Frost

Manage the proof state
-}
module Sift.Search.State where

import Control.Lens qualified as L
import Control.Lens.Operators ((+=), (<>=))
import Control.Monad.RWS qualified as M
import Extra
import Sift.Core.Monad
import Sift.Core.Types

deferGoal :: Logic e ()
deferGoal = do
  local <- M.get
  let lgoals = local ^. goals
  case lgoals of
    (a : b : r) -> do
      goals .= (b : a : r)
    _ -> pure ()

pushGoal :: Hypothesis e -> Logic e ()
pushGoal h = (goals <>= pure h)

popGoal :: Logic e (Hypothesis e)
popGoal = do
  local <- M.get
  let (h : hs) = local ^. goals
  goals .= hs
  pure h

freshIdx :: Logic e Idx
freshIdx = do
  state <- M.lift $ M.lift M.get
  let idx = state ^. generator
  M.lift $ M.lift $ generator += 1
  pure idx
