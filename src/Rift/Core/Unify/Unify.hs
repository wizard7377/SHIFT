{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Core.Unify.Unify where

import Control.Lens (makeLenses)
import Control.Lens qualified as Lens
import Data.Foldable
import Data.List (find)
import Extra.Basics
import Extra.Choice
import Extra.List
import Extra.Map
import Rift.Core.Base
import Rift.Core.Unify.Types

bind :: a -> a -> HImage a
bind a b = image a b
sbind :: (Applicative f) => a -> a -> f (HMap a)
sbind a b = simple [bind a b]

intros :: (Atomic atom) => Term atom -> QTerm atom
intros (Lamed a b) = let QTerm term frees = intros b in QTerm term $ a : frees
intros term = QTerm term []
unintros :: (Atomic atom) => QTerm atom -> Term atom
unintros (QTerm term frees) = foldr Lamed term frees

addTo :: (Atomic atom) => Term atom -> [Term atom] -> Term atom
addTo term = unintros . (mkQTerm term)
uTree :: Choice (Unification (Term atom)) -> [Term atom] -> [Term atom] -> UTree atom
uTree uni l r = fmap (\uni' -> (uni', UState l r)) uni
instance (Atomic atom) => Unify (Term atom) where
  unify t0 t1 =
    (sbind t0 t1) <||> case (t0, t1) of
      (Lamed b0 v0, Lamed b1 v1) -> unify b0 b1 <&&> unify v0 v1
      (Cons a0 b0, Cons a1 b1) -> unify a0 a1 <&&> unify b0 b1
      (Rule t0 f0, Rule t1 f1) -> unify t0 t1 <&&> unify f0 f1
      _ -> cabsurd

-- | Paritally apply a given reduction on some of the inputs
partApply :: (Atomic atom, ListLike m (Term atom), ListLike m (QTerm atom)) => (QTerm atom -> m (Term atom)) -> (QTerm atom -> m (QTerm atom))
partApply func term =
  let
    parts = subParts $ term ^. free
    vals = (\(outer, inner) -> (`mkQTerm` outer) <$> func (mkQTerm (term ^. root) inner)) <$> parts
   in
    fold vals

partApplyAuto :: (Atomic atom, ListLike m (Term atom), ListLike m (QTerm atom)) => (Term atom -> m (Term atom)) -> (Term atom -> m (Term atom))
partApplyAuto func term = unintros <$> partApply (func . unintros) (intros term)
