module Sift.Dev.Util where

import Control.Monad.Identity (Identity (runIdentity))
import Data.Text (pack)
import Rift (Term)
import Rift.Base (Atomic, Sentence)
import Rift.Dev.Paeno
import Sift (LMT, toSTerm)
import Sift.Base (LogicResult, defaultEnv)
import Sift.Monad (EnterState (enterState), mkLMT, runLMT)
import Sift.Solver.GenSearch (solve)

dummyRun :: (EnterState s Term, Atomic tok) => LMT (s Term tok) m a -> m a
dummyRun with = runLMT with defaultEnv (enterState defaultEnv [])
dummySolve :: (Atomic tok) => Term tok -> [Term tok] -> LogicResult ()
dummySolve goal given = runIdentity $ runLMT (solve $ toSTerm goal) defaultEnv (enterState defaultEnv given)

paxA = toSTerm pax0
paxB = toSTerm pax1
paxC = toSTerm pax2
paxD = toSTerm pax3
paxAA = toSTerm pax01
paxAB = toSTerm pax02
