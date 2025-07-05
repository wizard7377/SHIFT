module Sift.Ops.Fix where

import Control.Applicative (Alternative (..))
import Extra
import Rift qualified
import Sift.Core.Monad

fixReduce :: Redux
fixReduce t0@(Rift.Pe i t1) = pure (transform (change (Rift.Fe i) t0) t1)
fixReduce _ = empty
