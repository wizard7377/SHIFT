{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Rift.Unify (
  unify,
  Unification,
  (>?>),
  (>@>),
) where

import Rift.Base

import Control.Monad.Identity (Identity)
import Data.Kind (Type)
import Data.List (intersect, union)
import Data.Map
import Debug.Trace
import Extra.Choice
import Extra.List (forEach)
import Test.QuickCheck (collect)

{- | The basic unification function
unify :: (Eq (box tok), Ord (Term atom)) => Term atom -> Term atom -> Unification atom
-}
instance Unify Term where
  unify t0 t1 = case (t0, t1) of
    (Lamed b0 v0, Lamed b1 v1) -> unify b0 b1 <> unify v0 v1
    (Cons a0 b0, Cons a1 b1) -> unify a0 a1 <> unify b0 b1
    (Rule t0 f0, Rule t1 f1) -> unify t0 t1 <> unify f0 f1
    _ -> singleton t0 t1

mapToF :: (Ord a) => Map a a -> a -> a
mapToF m k = if member k m then m ! k else k

{- | The type of all unification, takes a list of bindings, then a list of free terms
 Note that the unification functions go from general to specific, so @unify t0 t1@ is saying that t0 is more general then t1
-}
type Unification term atom = (Ord (term atom)) => (Map (term atom) (term atom))

(>?>) :: (Unify term) => term atom -> term atom -> Unification term atom
v0 >?> v1 = unify v0 v1

(>@>) :: (Ord atom) => (Unify term) => Unification term atom -> term atom -> term atom
f >@> t = mapToF f t

class (forall a. (Ord a) => Ord (term a)) => Unify term where
  unify :: term atom -> term atom -> Unification term atom
infixr 5 >@>
infixr 6 >?>
