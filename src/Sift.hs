module Sift (module Sift.Base, module Sift.Monad, module Sift.Types, module Sift.Solver, module Sift.Solver.GenSearch) where

-- \^Solver for Flat Intuitionistic Types

import Sift.Base
import Sift.Monad hiding (_depth)
import Sift.Solver hiding (_depth, _sentences)
import Sift.Solver.GenSearch hiding (_depth)
import Sift.Types
