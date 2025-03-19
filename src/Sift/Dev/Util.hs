module Sift.Dev.Util where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (pack)
import Extra.Error (Error)
import Rift (Atomic, Term)
import Rift.Core.Dev.Paeno (pax0, pax01, pax02, pax1, pax2, pax3)
import Sift (LMT)
import Sift.Base (LogicResult, defaultEnv)
import Sift.Monad (EnterState (enterState), mkLMT, runLMT)
import Sift.Solver.GenSearch (SearchState (..), genSolve)

dummyRun :: (Monoid w, EnterState s Term, Atomic tok, Monad m) => LMT (s Term tok) w m a -> m (w, Either Error a)
dummyRun with = runLMT with defaultEnv (enterState defaultEnv [])

dummyRun' :: (Monoid w, EnterState SearchState Term, Atomic atom, Monad m) => LMT (SearchState Term atom) w m a -> m (w, Either Error a)
dummyRun' with = runLMT with defaultEnv (enterState defaultEnv [])

dummyWith :: (Monoid w, EnterState s Term, Atomic tok, Monad m) => LMT (s Term tok) w m a -> [Term tok] -> m (w, Either Error a)
dummyWith with given = runLMT with defaultEnv (enterState defaultEnv given)

-- dummySolve :: (Atomic tok) => Term tok -> [Term tok] -> ((), Either Error (LogicResult ()))
-- dummySolve goal given = runIdentity $ runLMT (genSolve goal) defaultEnv (enterState defaultEnv given)

instance (Atomic atom, Monoid w, EnterState s Term, Show w, Show res) => Show (LMT (s Term atom) w Identity res) where
  show = show . dummyRun
