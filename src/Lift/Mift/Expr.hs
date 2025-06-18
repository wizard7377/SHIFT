module Lift.Mift.Expr where

import Data.Text qualified as T
import Lift.Common.Names
import Lift.Common.Parsing

-- | A single Mift expreesion
data MiftExpr
  = -- | An expression tagged with it's source position
    MiftTagged (Lexical MiftExpr)
  | -- | An expression with
    MiftRepr T.Text MiftExpr
  | MiftLamed
  | -- | An unexpanded atom
    MiftAtom Name
  | MiftList [MiftExpr]
  | MiftCons MiftExpr MiftExpr
  | -- | Those with `${` `$}`
    MiftTuple [MiftExpr]
  | MiftApply MiftExpr MiftExpr
