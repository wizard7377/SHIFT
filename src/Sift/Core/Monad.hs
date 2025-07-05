{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Core.Monad where

import Control.Lens.Operators ((%=))
import Control.Monad.Accum qualified as M
import Control.Monad.Identity qualified as M
import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Text qualified as T
import Extra
import Extra.Choice.Types
import Rift qualified
import Sift.Core.Types
import Sift.Core.Unify

type LogicM e t r = ChoiceT (M.RWS (OpEnv e) (OpAccum) (OpState t)) r

type Redux e t = (Rift.TheoryOf t e) => t -> LogicM e t (Rift.TermOf e)
type Convert e t = (Rift.TheoryOf t e) => t -> t -> LogicM e t (Rift.TermOf e)
type Unify e t = (Rift.TheoryOf t e) => [t] -> t -> t -> LogicM e t (UnifyResult (Rift.TermOf e))

wrapApp :: (t -> LogicM e t r) -> LogicM e t r
wrapApp f = do
  state0 <- M.get
  res <- f (state0 ^. curTerm)
  pure res

runLogicM :: LogicM e t r -> OpEnv e -> OpState t -> OpAccum -> [r]
runLogicM m env state accum =
  M.execRWS (runChoiceT m) env accum state
    & M.runIdentity
    & M.execWriter
    & M.execState
    & M.runReader env
    & M.toList
