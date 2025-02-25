module Sift.Dev.Util where
import Sift.Types (LogicResult, defaultEnv)
import Rift (Term)
import Sift.Monad (mkLMT, EnterState (enterState), runLMT)
import Sift (LMT)
import Rift.Base (Sentence)
import Rift.Base (Token)
import Sift.Solver.GenSearch (solve)
import Control.Monad.Identity (Identity(runIdentity))

dummyRun :: (EnterState s,Sentence sen tok,Token tok) => LMT (s sen tok) m a -> m a 

dummyRun with = runLMT with defaultEnv (enterState defaultEnv [])
dummySolve :: Sentence Term tok => Term tok -> [Term tok] -> LogicResult ()
dummySolve goal given = runIdentity $ runLMT (solve goal) defaultEnv (enterState defaultEnv given)