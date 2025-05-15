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
import Extra.Error (Error)
import Rift (Atomic, LogicEnv, Sentence (..), Term', TermOf, TestTerm, TestToken, Theory, defaultEnv, getSentences, theory)
import Rift.Core.Base (Term, TermLike)
import Rift.Core.Dev (tRead)
import Rift.Core.Dev.Forms
import Sift (LMT, LogicResult (..))
import Sift.Monad (EnterState (..), applyEnv, mkLMT, runLMT, runLMT')
import Sift.Solver.GenSearch (SearchState (..), genSearch)

testRun :: (Monad m, Rift.TermOf (Rift.LogicEnv r) ~ Sift.Monad.TermOf s, Default r, Monoid w, EnterState s, Theory (LogicEnv r), Theory r) => LMT r w s m a -> m (Either Error a, s, w)
testRun with = applyEnv with Rift.defaultEnv

testRunWith :: forall term r w s m a. (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) => LMT [term] w s m a -> [term] -> m (Either Error a, s, w)
testRunWith comp t =
  let
    env0 :: Rift.LogicEnv [term] = defaultEnv
    env1 = set Rift.theory t env0
   in
    applyEnv comp env1

testSolve ::
  (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s) =>
  (v -> LMT [term] w s m a) ->
  [term] ->
  v ->
  m (Either Error a, s, w)
testSolve comp givens goal = testRunWith (comp goal) givens
benchSolve ::
  (Monad m, Monoid w, EnterState s, Theory (LogicEnv [term]), Rift.TermOf (Rift.LogicEnv [term]) ~ Sift.Monad.TermOf s, Control.DeepSeq.NFData (m (Either Error a, s, w))) =>
  (v -> LMT [term] w s m a) ->
  [term] ->
  v ->
  Benchmarkable
benchSolve comp givens goal = nf (\x -> testRunWith (comp x) givens) goal
