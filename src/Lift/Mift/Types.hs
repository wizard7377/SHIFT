module Lift.Mift.Types where

data MiftTerm a where
  MiftList :: [MiftTerm a] -> MiftTerm a
