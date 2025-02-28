{-# LANGUAGE BlockArguments #-}

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

-- | The basic unification function
unify :: (Eq atom, Ord (Term atom)) => Term atom -> Term atom -> Unification atom
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
type Unification atom = (Ord (Term atom)) => (Map (Term atom) (Term atom))

(>?>) :: (Eq atom) => Term atom -> Term atom -> Unification atom
v0 >?> v1 = unify v0 v1

(>@>) :: (Ord (Term atom)) => Unification atom -> Term atom -> Term atom
f >@> t = mapToF f t

infixr 5 >@>
infixr 6 >?>
