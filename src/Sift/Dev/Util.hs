module Sift.Dev.Util where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (pack)
import Extra.Error (Error)
import Rift (Atomic, Term, Term')
import Sift (LMT)
import Sift.Base (LogicResult, defaultEnv)
import Sift.Monad (EnterState (enterState), mkLMT, runLMT)
import Sift.Solver.GenSearch (SearchState (..), genSolve)

dummyRun :: (Monoid w, EnterState s, Atomic tok, Monad m) => LMT (s tok) w m a -> m (w, Either Error a)
dummyRun with = runLMT with defaultEnv (enterState defaultEnv [])

dummyRun' :: (Monoid w, Atomic atom, Monad m) => LMT (SearchState atom) w m a -> m (w, Either Error a)
dummyRun' with = runLMT with defaultEnv (enterState defaultEnv [])

dummyWith :: (Monoid w, EnterState s, Atomic tok, Monad m) => LMT (s tok) w m a -> [Term' tok] -> m (w, Either Error a)
dummyWith with given = runLMT with defaultEnv (enterState defaultEnv given)

-- dummySolve :: (Atomic tok) => Term' tok -> [Term' tok] -> ((), Either Error (LogicResult ()))
-- dummySolve goal given = runIdentity $ runLMT (genSolve goal) defaultEnv (enterState defaultEnv given)

instance (Atomic atom, Monoid w, EnterState s, Show w, Show res) => Show (LMT (s atom) w Identity res) where
  show = show . dummyRun
