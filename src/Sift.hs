module Sift (module Sift.Base, module Sift.Monad, module Sift.Types, module Sift.Solver) where

-- \^Solver for Flat Intuitionistic Types

import Sift.Base
import Sift.Monad hiding (_depth)
import Sift.Solver hiding (_depth, _sentences)
import Sift.Types
