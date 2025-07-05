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

reduceCyclic :: (Rift.Term (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> LogicM e (Rift.TermOf e) (Rift.TermOf e)
reduceCyclic t = reduce

reduceRecCyclic :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> LogicM e (Rift.TermOf t) (Rift.TermOf e)
reduceRecCyclic = reduceRec
