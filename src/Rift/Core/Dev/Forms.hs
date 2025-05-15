module Rift.Core.Dev.Forms where

import Rift.Core.Base
import Rift.Core.Instances

data TestSolve term = TestSolve
  { _system :: [term]
  , _givens :: [term]
  , _goal :: term
  }

deriving instance (Show a) => Show (TestSolve a)
deriving instance (Eq a) => Eq (TestSolve a)
