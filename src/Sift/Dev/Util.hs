{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Dev.Util where

import Control.Comonad (Comonad (..))
import Control.Exception (throw)
import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (pack)
import Extra.Error (Error)
import Rift (Atomic, Term, TestTerm, TestToken)
import Rift.Core.Dev (tRead)
import Rift.Core.Dev.Forms
import Sift (LMT, LogicResult (..))
import Sift.Base (LogicResult, defaultEnv)
import Sift.Monad (EnterState (enterState), mkLMT, runLMT, runLMT')
import Sift.Solver.GenSearch (SearchState (..), genSearch)

dummyRun :: (EnterState s) => LMT (s atom) w m a -> m (Either Error [a], s atom, w)
dummyRun with = runLMT' with defaultEnv (enterState defaultEnv [] [])

dummyWith :: (Monoid w, EnterState s, Atomic tok) => LMT (s tok) w m a -> [Term tok] -> m (Either Error [a], s tok, w)
dummyWith with given = runLMT' with defaultEnv (enterState defaultEnv given given)

dummySolve :: (Monoid w, EnterState s, Atomic tok) => (Term tok -> LMT (s tok) w m a) -> [Term tok] -> Term tok -> m (Either Error [a], s tok, w)
dummySolve with given goal = dummyWith (with goal) given

dummySolve2 with given goal = dummyWith (with (tRead given) (tRead goal)) []
testRSolve :: (Monoid w, EnterState s) => (TestTerm -> LMT (s TestToken) w m a) -> [TestTerm] -> String -> m (Either Error [a], s TestToken, w)
testRSolve with given goal = dummyWith (with $ tRead goal) given

testGenSolve :: (Ord a, Show a) => TestSolve a -> IO (_, _)
testGenSolve test =
  let
    r0 = extract $ dummySolve genSearch ((_givens test) ++ (_system test)) (_goal test)
   in
    case r0 of
      (Right v, s, w) -> _
      _ -> throw _

goodSolve :: (Comonad m) => m (Either Error [LogicResult], s tok, w) -> (Bool, s tok, w)
goodSolve m =
  let (res, state, writer) = extract m
      isGood = case res of Left _ -> False; Right v -> any (== Solved) v
   in (isGood, state, writer)
