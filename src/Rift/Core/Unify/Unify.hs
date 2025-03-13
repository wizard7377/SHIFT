{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Core.Unify.Unify where

import Control.Lens qualified as Lens
import Data.List (find)
import Extra.Choice
import Extra.List
import Extra.Map
import Rift.Core.Base

{- | The basic unification function
unify :: (Eq (box tok), Ord (Term atom)) => Term atom -> Term atom -> Unification atom
-}
type QTerm atom = (Term atom, [Term atom])

{- | The type of all unification, takes a list of bindings, then a list of free terms
 Note that the unification functions go from general to specific, so @unify t0 t1@ is saying that t0 is more general then t1
-}
type Binding a = Image a a

type Mapping a = Map a a

-- | May bind, must bind
type Qual a = ([a], [a])

bind :: a -> a -> Binding a
bind a b = image a b
sbind :: a -> a -> Choice (Mapping a)
sbind a b = simple [bind a b]

{- | The solver state
 In @forall x. (T0 -> forall a. A)@ & @forall y. (forall b. B -> T1)@...
-}
data UState atom = UState
  { _freeLeft :: [Term atom]
  -- ^ @x@
  , _freeRight :: [Term atom]
  -- ^ @y@
  }
  deriving (Eq, Ord)

deriving instance (Show atom, Show (Term atom)) => Show (UState atom)
Lens.makeLenses ''UState

-- | The type of bindings, usually notated \(sigma\)
type Unification a = HMap a

-- | A given state of a unification
type ULeaf atom = (Unification (Term atom), UState atom)

-- | A list of states of a unification
type UTree atom = Choice (ULeaf atom)

-- (>@>) :: (Unify (term atom)) => Unification (term atom) -> term atom -> term atom
-- f >@> t = mapToF f t

class Unify a where
  unify :: a -> a -> Choice (Unification a)

intros :: (Atomic atom) => Term atom -> QTerm atom
intros (Lamed a b) = let (term, frees) = intros b in (term, a : frees)
intros term = (term, [])
unintros :: (Atomic atom) => QTerm atom -> Term atom
unintros (term, frees) = foldr Lamed term frees

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
partApply' :: (Atomic atom) => (QTerm atom -> Term atom) -> (QTerm atom -> [QTerm atom])
partApply' func term@(root, free) =
  let
    parts = subParts free
    vals = (\(outer, inner) -> (unintros (root, inner), outer)) <$> parts
   in
    vals
