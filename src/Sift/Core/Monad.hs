{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Core.Monad (OpM, Redux', Redux, Convert, Convert', module Sift.Core.Types, OpM', runOpM, runOpM_, liftConvert, runOpMDef) where

import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Extra
import Rift qualified
import Sift.Core.Types

type OpM' t r = ChoiceT (M.RWS (OpEnv t) (OpAccum t) (OpState t)) r
type OpM t = OpM' t t
type Redux' t = (Rift.Inner t ~ t, Rift.Term t, Eq t, Show t, Rift.RTerm t) => t -> OpM t
type Convert' t = (Rift.Inner t ~ t, Rift.Term t, Eq t, Show t, Rift.RTerm t) => t -> t -> OpM' t Bool
type Redux = forall t. Redux' t
type Convert = forall t. Convert' t
runOpM_ :: OpM' t r -> (OpEnv t) -> ([r], OpState t, OpAccum t)
runOpM_ op env =
  let
    op' = (execChoiceT) op
   in
    M.runRWS op' env (opEnvToState env)

runOpM :: OpM' t r -> (OpEnv t) -> [r]
runOpM op env =
  let
    (result, _, _) = runOpM_ op env
   in
    result

runOpMDef :: OpM' t r -> [r]
runOpMDef f = runOpM f def
liftConvert :: Convert' t -> Redux' t
liftConvert f x = do
  state <- M.get
  let Just goal = state ^. curGoal
  (cguardM (f x goal)) >> pure x

testConvert :: (Rift.Inner t ~ t, Rift.KTerm t, Rift.FTerm t, Rift.UTerm Int t, Plated t, Eq t, Show t, Rift.RTerm t) => Convert' t -> t -> t -> Bool
testConvert f x goal =
  let
    (result, _, _) = runOpM_ (f x goal) def
   in
    or result
