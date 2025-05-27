{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Dev.Util where

import Control.Comonad (Comonad (..))
import Control.DeepSeq qualified
import Control.Exception (throw)
import Control.Lens (set)
import Control.Monad.Identity (Identity (runIdentity))
import Criterion
import Data.Default (Default)
import Data.Text (pack)
import Data.Text qualified as T
import Extra.Error (Error (..))
import GHC.Exts (Any)
import Rift (Atomic, LogicEnv, LogicResult (..), Sentence (..), Term', TermOf, TestTerm, TestToken, Theory, defaultEnv, getSentences, theory)
import Rift.Core.Base (Term, TermLike)
import Rift.Core.Dev (tRead)
import Rift.Core.Dev.Forms
import Sift.Monad (EnterState (..), FormsLMT, LMT, applyEnv, mkLMT, runLMT, runLMT')
import Sift.Solver (SearchState (..), genSearch)
import Test.HUnit qualified as T
import Test.HUnit.Lang (HUnitFailure (HUnitFailure))

testRun :: (Monad m, Rift.TermOf (Rift.LogicEnv r) ~ Sift.Monad.TermOf s, Default r, Monoid w, EnterState s, Theory (LogicEnv r), Theory r) => LMT r w s m a -> m (Either Error a, s, w)
testRun with = applyEnv with Rift.defaultEnv

testRunWith :: forall term r w s m a. (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) => LMT [term] w s m a -> [term] -> m (Either Error a, s, w)
testRunWith comp !t =
  let
    env0 :: Rift.LogicEnv [term] = defaultEnv
    env1 = set Rift.theory t env0
   in
    applyEnv comp env1

testSolve ::
  (Sift.Monad.FormsLMT [term] w s m term) =>
  (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) =>
  (v -> LMT [term] w s m a) ->
  [term] ->
  v ->
  m (Either Error a, s, w)
testSolve comp !givens !goal = testRunWith (comp goal) givens
benchSolve ::
  (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s, Control.DeepSeq.NFData (m (Either Error a, s, w))) =>
  (v -> LMT [term] w s m a) ->
  [term] ->
  v ->
  Benchmarkable
benchSolve comp givens = nf (\x -> testRunWith (comp x) givens)
benchSolve' ::
  (Monad m, Monoid (), EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) =>
  (v -> LMT [term] () s m a) ->
  [term] ->
  v ->
  Benchmarkable
benchSolve' comp givens goal = whnf (\x -> testRunWith (comp x) givens) goal
requireSolve ::
  (Monad m, Monoid w, Comonad m, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) =>
  (v -> LMT [term] w s m (Rift.LogicResult term)) ->
  [term] ->
  v ->
  IO (s, w)
requireSolve comp givens goal = do
  (result, state, w) <- pure $ extract $ testRunWith (comp goal) givens
  case result of
    Left err -> throw err
    Right (Rift.Solved result) -> return (state, w)
    _ -> (T.assertFailure "")
requireSolve' ::
  (Monad m, Monoid (), Comonad m, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s, Show term, Show v) =>
  (v -> LMT [term] () s m (Rift.LogicResult term)) ->
  [term] ->
  v ->
  IO (s, ())
requireSolve' comp givens goal = do
  (result, state, w) <- pure $ extract $ testRunWith (comp goal) givens
  case result of
    Left err -> throw err
    Right (Rift.Solved proof) -> return (state, w)
    _ -> (throw (Other (T.pack $ "Cannot deduce " <> show goal <> " from: " <> show givens)))

getResult :: forall m a. (Monad m) => forall w s. (Monoid w) => m (Either Error a, w, s) -> m (Either Error a)
getResult m = do
  (result, _, _) <- m
  return result
