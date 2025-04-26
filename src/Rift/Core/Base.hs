{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Rift.Core.Base (BasicAtom (..), Term (..), Term', Sentence (..), TestTerm, Atomic, lamed, cons, rule, pattern PCons, pattern Lamed, pattern Rule, pattern Cons, termToList, listToTerm, mkCons, mkCons') where

import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()

data Term atom where
  TAtom :: atom -> Term atom
  TCons :: Term atom -> Term atom -> Term atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord, Eq)

data BasicAtom atom = ALamed | ARule | AAtom atom
  deriving (Data, Generic, Ord, Eq)

instance (Show atom) => Show (BasicAtom atom) where
  show (AAtom a) = show a
  show ALamed = "?"
  show ARule = ":"

-- | The type of all abstract terms in SHIFt
-- Note that this definition is quite minimal, concrete implementation details are hidden from difference parts of the compiler by the @atom@ parameter
type Term' atom = Term (BasicAtom atom)

type TestTerm = Term' Text

-- | The attomic class constraint
-- Represents a collection of "packaged requirements" that all atoms must have
-- All of these are fairly standard and should be implemented for most types anyway
type Atomic a = (Eq a, Ord a, Show a)

-- type Atom a = (Eq a, Show a)

-- | The class of all top level sentences, which can convert to `Term' tok`
class Sentence sen term | sen -> term where
  fromTerm :: term atom -> sen atom
  toTerm :: sen atom -> term atom

instance Sentence a a where
  fromTerm = id
  toTerm = id

mkAtom = TAtom

atom :: atom -> Term' atom
atom = TAtom . AAtom

mkCons :: Term' atom -> Term' atom -> Term' atom -> Term' atom
mkCons f a0 a1 = TCons f (TCons a0 a1)

mkCons' :: (BasicAtom atom) -> Term' atom -> Term' atom -> Term' atom
mkCons' f = mkCons (TAtom f)

cons :: Term' atom -> Term' atom -> Term' atom
cons = TCons

lamed = mkCons' ALamed

rule = mkCons' ARule

pattern PCons :: Term' atom -> Term' atom -> Term' atom -> Term' atom
pattern PCons f a0 a1 <- TCons f (TCons a0 a1)
  where
    PCons f a0 a1 = mkCons f a0 a1

pattern Cons :: Term' atom -> Term' atom -> Term' atom
pattern Cons a0 a1 <- (TCons a0 a1)
  where
    Cons a0 a1 = cons a0 a1

pattern Lamed :: Term' atom -> Term' atom -> Term' atom
pattern Lamed a0 a1 <- TCons (TAtom ALamed) (TCons a0 a1)
  where
    Lamed a0 a1 = lamed a0 a1

pattern Rule :: Term' atom -> Term' atom -> Term' atom
pattern Rule a0 a1 <- TCons (TAtom ARule) (TCons a0 a1)
  where
    Rule a0 a1 = rule a0 a1

{-
pattern Cons :: Term' atom -> Term' atom -> Term' atom
pattern Cons a0 a1 <- TCons (TAtom ACons) (TCons a0 a1)
  where
    Cons a0 a1 = cons a0 a1

-}
-- pattern Yud :: Term' atom
-- pattern Yud <- TAtom AYud
--  where
--    Yud = TAtom AYud

-- yud :: Term' atom
-- yud = TAtom AYud
termToList :: Term' atom -> [Term' atom]
termToList (TCons a0 a1) = (:) a0 $ termToList a1
termToList _ = []

listToTerm :: [Term' atom] -> Maybe (Term' atom)
listToTerm [] = Nothing
listToTerm [x] = Just x
listToTerm (x : xs) = cons x <$> (listToTerm xs)
