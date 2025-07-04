{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lift.Mift.Parse.Instances where

import Lift.Common.Module (ParseState)
import Lift.Mift.Base
import Lift.Mift.Expr (MiftExpr)
import Rift qualified

instance Rift.Language IO (MiftM t) where
  type ResultOfL (MiftM t) = t
  parseFile l env file = _
  parseText l env text = _
