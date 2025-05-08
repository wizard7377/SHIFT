{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Base (
  {-# WARNING "Do not use kernel forms, use abstracted versions instead" #-} module Rift.Core.Kernel,
  Term,
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
  recurseSome
) where

import Rift.Core.Kernel
import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()

{-| The type of all abstract terms in SHIFT 
 - @Term@ itself is parameterized over a set of atoms, ת, which define the lexicon of whatever system is being used 
 - For instance, if we define the atomic type to be `Text`, we are stating that we are dealing with a lexicon of all text values. 
 -
 - In addition to this, we also know define a ל primitive, which must exist for the system to be useful
 -
 - This is defined to seperate the internals of `PrimTerm` from the rest of the system. 
 - To further this, each of the constructors of `PrimTerm` has both a pattern synomn as well as a simple functional constructor 
 - These also abstract the specific nature, at least in the case of ל. 
 - That is, instead of having to use the actual `PrimLamed`, we instead use a ternary constructor
 - The equalivents are:
 - * For `PrimLamed`, `Lamed` and `lamed`
 - * For `PrimCons`, `Cons` and `cons``
 - * For `PrimAtom`, `Atom` and `atom`
 - -}
type Term = PrimTerm

data TestToken = TestToken (Either Text Int)
  deriving (Eq, Ord, Data, Generic)

type TestTerm = Term TestToken
{- | The attomic class constraint
Represents a collection of "packaged requirements" that all atoms must have
All of these are fairly standard and should be implemented for most types anyway
-}
type Atomic a = (Eq a, Ord a, Show a)

-- type Atom a = (Eq a, Show a)
instance TermLike atom (Term atom) where
  toTerm = id

{-# DEPRECATED #-}
mkAtom = PrimAtom

-- |The functional version of `PrimAtom`, equavilent to `Atom`
atom :: atom -> Term atom
atom = PrimAtom

{-# DEPRECATED #-}
mkCons3 :: Term atom -> Term atom -> Term atom -> Term atom
mkCons3 f a0 a1 = PrimCons f (PrimCons a0 a1)

{-# DEPRECATED #-}
mkCons :: Term atom -> Term atom -> Term atom
mkCons = PrimCons
-- |The functional version of `PrimCons`, equaivelent to `Cons`
cons :: Term atom -> Term atom -> Term atom
cons = PrimCons

-- |The functional version of `PrimLamed`, equaivelent to `Lamed`
lamed ::
  -- |The variable
  Term atom ->
  -- |The consequent
  Term atom ->
  -- |The precedent
  Term atom ->
  -- |The result
  Term atom
lamed frees a0 a1 = PrimCons PrimLamed (PrimCons frees (PrimCons a0 a1))

-- FOR TESTS ONLY, THIS IS A PARTIAL FUNCTION
manyLamed :: [Term atom] -> Term atom -> Term atom -> Term atom
manyLamed [t] a0 a1 = lamed t a0 a1
manyLamed (t : ts) a0 a1 = lamed t (manyLamed ts a0 a1) a1
--drule = mkCons' ARule

pattern Cons3 :: Term atom -> Term atom -> Term atom -> Term atom
pattern Cons3 f a0 a1 <- PrimCons f (PrimCons a0 a1)
  where
    Cons3 f a0 a1 = mkCons3 f a0 a1

-- |The pattern synonym for atom
pattern Atom :: atom -> Term atom
pattern Atom a0 <- PrimAtom a0
  where
    Atom a0 = PrimAtom a0

-- |The pattern sysnonym for cons
pattern Cons :: Term atom -> Term atom -> Term atom
pattern Cons a0 a1 <- (PrimCons a0 a1)
  where
    Cons a0 a1 = cons a0 a1


-- |The pattern synoynm for lamed
pattern Lamed :: Term atom -> Term atom -> Term atom -> Term atom
pattern Lamed a0 a1 a2 <- PrimCons PrimLamed (PrimCons a0 (PrimCons a1 a2))
  where
    Lamed a0 a1 a2 = lamed a0 a1 a2

termToList :: Term atom -> [Term atom]
termToList (PrimCons a0 a1) = (:) a0 $ termToList a1
termToList _ = []

listToTerm :: [Term atom] -> Maybe (Term atom)
listToTerm [] = Nothing
listToTerm [x] = Just x
listToTerm (x : xs) = cons x <$> listToTerm xs

-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseSome :: (Eq atom) => (Term atom -> Term atom) -> (Term atom -> Term atom)
recurseSome f v = let v' = f v in
  if v == v' then (
    case v of
      PrimCons a0 a1 -> PrimCons (recurseSome f a0) (recurseSome f a1)
      PrimAtom _ -> v
  )
    else v'


