{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

module Lift.Mift.Expr where

import Control.Lens qualified as Lens
import Data.List (intercalate)
import Data.Text qualified as T
import Extra
import Lift.Common.Names
import Lift.Common.Parsing
import Rift qualified

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
  | -- | ∀x. A
    MiftDalet [MiftExpr] MiftExpr
  | MiftWild
  | MiftVoid
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show MiftExpr where
  show (MiftTagged (Lexical _ e)) = show e
  show (MiftRepr t e) = T.unpack t <> "@" <> show e
  show MiftLamed = "ל"
  show (MiftAtom (Name n)) = T.unpack n
  show (MiftList xs) = "[" <> intercalate ", " (map show xs) <> "]"
  show (MiftCons x y) = show x <> " . " <> show y
  show (MiftTuple xs) = "{" <> intercalate ", " (map show xs) <> "}"
  show (MiftApply f x) = show f <> " $ " <> show x
  show (MiftDalet xs e) =
    "∀" <> intercalate ", " (map show xs) <> ". " <> show e
  show MiftWild = "$_"
  show MiftVoid = "⊥"

maybeKaf :: MiftExpr -> Maybe (Rift.PrimKaf MiftExpr)
maybeKaf (MiftCons t0 t1) = Just (Rift.PrimKafCon t0 t1)
maybeKaf (MiftList (head : rest)) = Just (Rift.PrimKafCon head (MiftList rest))
maybeKaf _ = Nothing
instance Rift.KTerm MiftExpr where
  pKaf :: Prism' MiftExpr (Rift.PrimKaf MiftExpr)
  pKaf = Lens.prism' (\(Rift.PrimKafCon t0 t1) -> MiftCons t0 t1) maybeKaf
  isLamed :: MiftExpr -> Bool
  isLamed = (== MiftLamed)
  mkLamed :: MiftExpr
  mkLamed = MiftLamed

instance Rift.ETerm MiftExpr where
  eterm :: MiftExpr
  eterm = MiftVoid

setMiftTerm (MiftDalet frees term) inner = MiftDalet frees (setMiftTerm term inner)
setMiftTerm _ inner = inner
instance Rift.FTerm MiftExpr where
  type Inner MiftExpr = MiftExpr
  fterm ::
    (Rift.KTerm (Rift.Inner MiftExpr)) =>
    Lens' MiftExpr (Rift.Inner MiftExpr)
  fterm = _
  frees ::
    (Rift.KTerm (Rift.Inner MiftExpr)) =>
    Lens' MiftExpr [Rift.Inner MiftExpr]
  frees = _
  groundTerm :: Rift.Inner MiftExpr -> MiftExpr
  groundTerm = _
