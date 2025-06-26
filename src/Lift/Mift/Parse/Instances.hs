{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Lift.Mift.Parse.Instances where

import Lift.Common.Module (ParseState)
import Lift.Mift.Expr (MiftExpr)
import Rift qualified

instance (Rift.Theory (ParseState MiftExpr)) => Rift.UserTheory (ParseState MiftExpr) where
  type MonadOf (ParseState MiftExpr) = IO
  parseTerm input = _
  printTerm = _
