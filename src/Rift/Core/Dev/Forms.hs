module Rift.Core.Dev.Forms where

import Rift.Core.Base
import Rift.Core.Instances

data TestSolve a = TestSolve
  { _system :: [Term' a]
  , _givens :: [Term' a]
  , _goal :: Term' a
  }

deriving instance (Show a) => Show (TestSolve a)
deriving instance (Eq a) => Eq (TestSolve a)
