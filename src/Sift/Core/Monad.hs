{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Core.Monad (OpM, Redux', Redux, Convert, Convert', Unify', Unify, module Sift.Core.Types, OpM', runOpM, runOpM', runOpM_, liftConvert, runOpMDef) where

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
import Rift qualified
import Sift.Core.Types
import Sift.Core.Unify

type OpM' thy r = (Rift.Term (Rift.TermOf thy), Rift.Theory thy) => ChoiceT (M.RWS (OpEnv thy) (OpAccum) (OpState (Rift.TermOf thy))) r
type OpM thy = OpM' thy (Rift.TermOf thy)
type Redux' thy = (Rift.Theory thy, Rift.Term (Rift.TermOf thy)) => (Rift.TermOf thy) -> OpM' (thy) (Rift.TermOf thy)
type Convert' thy r = (Rift.Theory thy) => (Rift.TermOf thy) -> (Rift.TermOf thy) -> OpM' (thy) r
type Redux = forall thy. Redux' thy
type Convert r = forall thy. (Rift.Theory thy) => Convert' thy r
type Unify' t = (Rift.Theory t) => Convert' t (UnifyState (Rift.TermOf t))
type Unify = forall t. Unify' t

tellOp :: (Show r) => OpTypes -> OpM' t r -> OpM' t r
tellOp opType op = do
  v <- op
  lift $ M.tell $ OpAccum [(opType, T.pack $ show v)]
  pure v
runOpM_ :: (Rift.Theory thy) => OpM' thy r -> (OpEnv thy) -> ([r], OpState (Rift.TermOf thy), OpAccum)
runOpM_ op env =
  let
    op' = (execChoiceT) op
   in
    M.runRWS op' env (opEnvToState env)

runOpM :: (Rift.Theory t) => OpM' t r -> (OpEnv t) -> [r]
runOpM op env =
  let
    (result, _, _) = runOpM_ op env
   in
    result

runOpM' :: (Rift.Theory t) => OpM' t r -> t -> Maybe (Rift.TermOf t) -> [r]
runOpM' f x goal =
  let
    state = OpEnv def (x) goal
   in
    runOpM f state
runOpMDef :: forall t r. (Rift.Theory t, Rift.Term (Rift.TermOf t), Default t) => OpM' t r -> [r]
runOpMDef f = runOpM f (def :: OpEnv t)

liftConvert :: (Rift.TheoryOf t thy, Rift.Term t) => Convert' thy r -> (t -> OpM' thy r)
liftConvert f x = do
  state <- M.get
  let Just goal = state ^. curGoal
  f x goal
testConvert :: forall thy r. (Rift.Theory thy) => ((Rift.TermOf thy) -> OpM' thy r) -> (Rift.TermOf thy) -> (Rift.TermOf thy) -> thy -> [r]
testConvert f x goal' thy =
  let
    result = runOpM (f x) (OpEnv def thy (Just goal'))
   in
    result
