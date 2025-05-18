{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Rift.Core.Base (
  Term(..),
  AnyTerm,
  Term',
  TestTerm,
  TestToken (..),
  Atomic,
  lamed,
  cons,
  --drule,
  pattern Cons3,
  pattern Lamed,
  pattern Atom,
  --pattern Rule,
  pattern Cons,
  termToList,
  listToTerm,
  mkCons,
  mkCons3,
  manyLamed,
  TermLike,
  recurseSome,
  poccurs,
  {-# WARNING "Don't use kernel forms, abstract instead" #-} module Rift.Core.Kernel
) where

import Rift.Core.Kernel
import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()

data Term' atom where
  PrimLamed :: Term' atom
  PrimAtom :: atom -> Term' atom
  PrimCons :: Term' atom -> Term' atom -> Term' atom
  deriving (Eq, Ord)

instance (Term (Term' atom)) where
  type AtomOf (Term' atom) = atom
  viewTerm (PrimCons t0 t1) = PrimConsCon t0 t1
  viewTerm (PrimAtom a) = PrimAtomCon a
  viewTerm PrimLamed = PrimLamedCon
  makeTerm (PrimConsCon t0 t1) = PrimCons t0 t1
  makeTerm (PrimAtomCon a) = PrimAtom a
  makeTerm PrimLamedCon = PrimLamed
type TermLike term = (Eq term, Ord term, Show term)
data TestToken = TestToken (Either Text Int)
  deriving (Eq, Ord, Data, Generic)

type TestTerm = Term' TestToken
{- | The attomic class constraint
Represents a collection of "packaged requirements" that all atoms must have
All of these are fairly standard and should be implemented for most types anyway
-}
type Atomic a = (Eq a, Ord a, Show a)
-- |The functional version of `PrimAtom`, equavilent to `Atom`
atom :: Term term => (AtomOf term) -> term
atom = Atom

{-# DEPRECATED #-}
mkCons3 :: AnyTerm term => term->term->term-> term
mkCons3 f a0 a1 = Cons f (Cons a0 a1)

{-# DEPRECATED #-}
mkCons :: AnyTerm term => term->term-> term
mkCons = Cons
-- |The functional version of `Cons`, equaivelent to `Cons`
cons :: AnyTerm term => term->term-> term
cons = Cons

-- |The functional version of `PrimLamed`, equaivelent to `Lamed`
lamed ::
  Term term =>
  -- |The variable
  term ->
  -- |The consequent
  term ->
  -- |The precedent
  term ->
  -- |The result
  term
lamed frees a0 a1 = Cons BasicLamed (Cons frees (Cons a0 a1))

-- FOR TESTS ONLY, THIS IS A PARTIAL FUNCTION
manyLamed :: Term term => [term] -> term -> term -> term
manyLamed [t] a0 a1 = Lamed t a0 a1
manyLamed (t : ts) a0 a1 = Lamed t (manyLamed ts a0 a1) a1
--drule = mkCons' ARule


pattern Cons3 :: Term term => term -> term -> term -> term
pattern Cons3 f a0 a1 <- Cons f (Cons a0 a1)
  where
    Cons3 f a0 a1 = Cons f (Cons a0 a1)

pattern Lamed :: Term term => term -> term -> term -> term
pattern Lamed v a b <- Cons BasicLamed (Cons v (Cons a b))
  where
    Lamed v a b = Cons BasicLamed (Cons v (Cons a b))
{-# INLINE Lamed #-}
termToList :: Term term => term -> [term]
termToList (Cons a0 a1) = (:) a0 $ termToList a1
termToList _ = []

listToTerm :: AnyTerm term => [term] -> Maybe (term)
listToTerm [] = Nothing
listToTerm [x] = Just x
listToTerm (x : xs) = Cons x <$> listToTerm xs

-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseSome :: (Eq term, Term term) => (term -> term) -> term -> term
recurseSome f v = let v' = f v in
  if v == v' then (
    case v of
      Cons a0 a1 -> Cons (recurseSome f a0) (recurseSome f a1)
      Atom _ -> v
      BasicLamed -> BasicLamed
  )
    else v'

-- |Check if a given term occurs within another term
poccurs :: (Eq term, Term term) => 
  -- |The larger term
  term -> 
  -- |The smaller term
  term -> Bool 
poccurs term value = 
  (value == term) || (case term of
    Cons a0 a1 -> (poccurs term a0) || (poccurs term a1)
    Atom _ -> False
    BasicLamed -> False)
