module Sift.Types where 

data LogicResult e = 
    Solved 
    | Unsolved 
    | Stopped 
    | Failed e

