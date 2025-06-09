module Lift.Mift.Base where

import Control.Monad.Identity (Identity)
import Data.Text qualified as T
import Lift.Common.Names
import Lift.Common.Parsing (ParseMT)
import Lift.Mift.Expr (MiftExpr)

type MiftM a = ParseMT (ParseInfo MiftExpr) MiftExpr Identity a
data ParseInfo t
  = NoSymbol Name
  | AlreadyDefined Name
