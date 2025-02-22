module Sift.Solver.BasicSearch.Tree where

import Data.Tree 

data SLabel a seq = 
    Statement a 
    | Sequence seq 
    | SolveAll