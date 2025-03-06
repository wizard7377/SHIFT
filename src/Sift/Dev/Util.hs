module Sift.Dev.Util where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (pack)
import Extra.Error (Error)
import Rift (Atomic, Term)
import Rift.Core.Dev.Paeno (pax0, pax01, pax02, pax1, pax2, pax3)
import Sift (LMT, toSTerm)
import Sift.Base (LogicResult, defaultEnv)
import Sift.Monad (EnterState (enterState), mkLMT, runLMT)
import Sift.Solver.GenSearch (solve)

dummyRun :: (Monoid w, EnterState s Term, Atomic tok, Monad m) => LMT (s Term tok) w m a -> m (w, Either Error a)
dummyRun with = runLMT with defaultEnv (enterState defaultEnv [])
dummySolve :: (Atomic tok) => Term tok -> [Term tok] -> ((), Either Error (LogicResult ()))
dummySolve goal given = runIdentity $ runLMT (solve goal) defaultEnv (enterState defaultEnv given)

paxA = toSTerm pax0
paxB = toSTerm pax1
paxC = toSTerm pax2
paxD = toSTerm pax3
paxAA = toSTerm pax01
paxAB = toSTerm pax02
