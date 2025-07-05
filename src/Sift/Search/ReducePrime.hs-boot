module Sift.Search.ReducePrime where

import Rift qualified
import Sift.Core.Monad

reduceCyclic :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => p -> t -> OpM t
reduceRecCyclic :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => (Rift.Inner t ~ t) => t -> OpM t
