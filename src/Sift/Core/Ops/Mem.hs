module Sift.Core.Ops.Mem where

import Extra
import Rift qualified
import Sift.Core.Unify

memReduce :: (Rift.Term t) => t -> Choice t
memReduce term =
  _
 where
  ground = term ^. Rift.groundTerm
  frees = term ^. Rift.frees
