{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Rift.Core.Unify (
  unify,
  Unification,
) where

import Rift.Core.Base

import Control.Monad.Identity (Identity)
import Data.Fixed (Uni)
import Data.Kind (Type)
import Data.List (find, intersect, union)
import Data.Map
import Debug.Trace
import Extra.Basics
import Extra.Choice
import Extra.List (forEach)
import Test.QuickCheck (collect)

{- | The basic unification function
unify :: (Eq (box tok), Ord (Term atom)) => Term atom -> Term atom -> Unification atom
-}
instance (Atomic atom) => Unify (Term atom) where
  unify t0 t1 =
    (sbind t0 t1) <||> case (t0, t1) of
      (Lamed b0 v0, Lamed b1 v1) -> unify b0 b1 <&&> unify v0 v1
      (Cons a0 b0, Cons a1 b1) -> unify a0 a1 <&&> unify b0 b1
      (Rule t0 f0, Rule t1 f1) -> unify t0 t1 <&&> unify f0 f1
      _ -> cabsurd

mapToF :: (Ord a) => Map a a -> a -> a
mapToF m k = if member k m then m ! k else k
bindLook :: (Atomic a) => [Binding a] -> a -> Maybe a
bindLook binds val = snd <$> find (\(x, y) -> x == val) binds
bindToF :: (Atomic a) => [Binding a] -> (a -> a)
bindToF vals = \input -> case bindLook vals input of
  Just val -> val
  Nothing -> input

{- | The type of all unification, takes a list of bindings, then a list of free terms
 Note that the unification functions go from general to specific, so @unify t0 t1@ is saying that t0 is more general then t1
-}
type Binding a = (a, a)

-- | May bind, must bind
type Qual a = ([a], [a])

bind :: a -> a -> Binding a
bind a b = (a, b)
sbind :: a -> a -> Choice (Binding a)
sbind a b = simple $ bind a b
type Unification a = Choice (Binding a)

check ::
  (Atomic a) =>
  -- | A list of free terms in the first term
  [a] ->
  -- | A list of bound terms in the first term
  [a] ->
  -- | A list of free terms in the second term
  [a] ->
  -- | A list of bound terms in the second term
  [a] ->
  Unification a ->
  Unification a
check fA rA fB rB cbinds =
  solve
    ( \binds ->
        -- TODO expand for hetrogenues he in other
        iso binds
          && all
            ( \(from, to) ->
                ( if from `elem` rA || to `elem` rB
                    then
                      (from `elem` rA && to `elem` fB) || (from `elem` fA && to `elem` rB)
                    else
                      (from == to) || (from `elem` fA || to `elem` fB)
                )
            )
            binds
    )
    cbinds

-- (>@>) :: (Unify (term atom)) => Unification (term atom) -> term atom -> term atom
-- f >@> t = mapToF f t
apply :: (Atomic a) => Unification (Term a) -> Term a -> Term a
apply = _
class Unify a where
  unify :: a -> a -> Unification a
