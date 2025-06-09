module Sift (module Sift.Core, module Sift.Base, module Sift.Monad, module Sift.Solver) where

-- \^Solver for Flat Intuitionistic Types

import Sift.Base
import Sift.Core
import Sift.Monad hiding (_depth)
import Sift.Solver hiding (_depth, _sentences)
