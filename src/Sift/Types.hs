module Sift.Types where 
import qualified Rift


-- |The result of a logic solve
data LogicResult e = 
    -- |Solved
    Solved 
    -- |Proven unsolvable (only possible in certain solvers)
    | Unsolved 
    -- |Stopped, most likely due to overrunning @_depth@
    | Stopped 
    -- |Failed with an error
    | Failed e

-- |The enviroment of a logical solve 
data LogicEnv = LogicEnv {
    -- |The depth it is allowed to go to
    _depth :: Maybe Int
}

defaultEnv :: LogicEnv 
defaultEnv = LogicEnv {
    _depth = Just 30
}