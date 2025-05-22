module Lift.Mift.Types where

import Lift.Core.Symbol.Types

data MiftTerm where
  MiftAtom :: Symbol -> MiftTerm
  MiftList :: [MiftTerm] -> MiftTerm
  MiftLamed :: MiftTerm
