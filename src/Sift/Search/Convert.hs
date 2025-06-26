
module Sift.Search.Convert where

import Control.Applicative (Alternative (..))
import Sift.Ops.Common
import Sift.Ops.Simple

convert :: Convert' t
convert t0 t1 =
  nullConvert t0 t1