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
import Data.Choice
import Rift qualified
import Sift.Core.Types
import Sift.Core.Unify

type Logic e = (Rift.Theory e) => LogicM e (Rift.TermOf e)
type LogicM e r = ChoiceT (M.RWS (OpEnv e) (OpAccum) OpState) r

type Redux e t = (Rift.TheoryOf t e) => t -> LogicM e (Rift.TermOf e)
type Convert e t = (Rift.TheoryOf t e) => t -> t -> LogicM e (Rift.TermOf e)
type Unify e t = (Rift.TheoryOf t e) => [t] -> t -> t -> LogicM e (UnifyResult (Rift.TermOf e))

runLogicM :: (Rift.TheoryOf t e) => LogicM e r -> OpEnv e -> [r]
runLogicM m env =
  let
    r0 = execChoiceT m
    (r1, _, _) = (M.runRWS r0 env (opEnvToState env))
   in
    r1
