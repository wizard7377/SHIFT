module Sift.Search.ReducePrime where

import Control.Applicative (Alternative (..))
import Control.Monad.Morph
import Control.Monad.Reader
import Extra
import Extra.Choice
import Rift qualified
import Sift.Ops.Common
import Sift.Ops.Mem
import Sift.Ops.Zeta
import Sift.Search.Convert
import Sift.Search.Reduce

reduceCyclic :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => p -> t -> OpM t
reduceCyclic t = reduce
reduceRecCyclic :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => (Rift.Inner t ~ t) => t -> OpM t
reduceRecCyclic = reduceRec
