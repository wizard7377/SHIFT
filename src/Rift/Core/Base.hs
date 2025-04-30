{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Rift.Core.Base (
  {-# WARNING "Do not use kernel forms, use abstracted versions instead" #-} module Rift.Core.Kernel,
  Term',
  TestTerm,
  Sentence (..),
  Atomic,
  lamed,
  cons,
  TestToken,
  --drule,
  pattern PCons,
  pattern Lamed,
  --pattern Rule,
  pattern Cons,
  termToList,
  listToTerm,
  mkCons,
  mkCons',
  manyLamed
) where

import Rift.Core.Kernel
import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()

{- | The type of all abstract terms in SHIFt
Note that this definition is quite minimal, concrete implementation details are hidden from difference parts of the compiler by the @atom@ parameter
-}
type Term' atom = Term (BasicAtom atom)

type TestToken = Either Text Int 
type TestTerm = Term' TestToken

{- | The attomic class constraint
Represents a collection of "packaged requirements" that all atoms must have
All of these are fairly standard and should be implemented for most types anyway
-}
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

lamed :: Term' atom -> Term' atom -> Term' atom -> Term' atom
lamed frees a0 a1 = TCons (TAtom ALamed) (TCons frees (TCons a0 a1))

-- FOR TESTS ONLY, THIS IS A PARTIAL FUNCTION
manyLamed :: [Term' atom] -> Term' atom -> Term' atom -> Term' atom
manyLamed [t] a0 a1 = lamed t a0 a1
manyLamed (t : ts) a0 a1 = lamed t (manyLamed ts a0 a1) a1
--drule = mkCons' ARule

pattern PCons :: Term' atom -> Term' atom -> Term' atom -> Term' atom
pattern PCons f a0 a1 <- TCons f (TCons a0 a1)
  where
    PCons f a0 a1 = mkCons f a0 a1

pattern Atom :: atom -> Term' atom
pattern Atom a0 <- TAtom (AAtom a0)
  where
    Atom a0 = TAtom (AAtom a0)
pattern Cons :: Term' atom -> Term' atom -> Term' atom
pattern Cons a0 a1 <- (TCons a0 a1)
  where
    Cons a0 a1 = cons a0 a1

pattern DLamed :: Term' atom -> Term' atom -> Term' atom
pattern DLamed a0 a1 <- TCons (TAtom ALamed) (TCons a0 a1)
  where
    DLamed a0 a1 = _
    --DLamed a0 a1 = lamed a0 a1

pattern Lamed :: Term' atom -> Term' atom -> Term' atom -> Term' atom
pattern Lamed a0 a1 a2 <- TCons (TAtom ALamed) (TCons a0 (TCons a1 a2))
  where
    Lamed a0 a1 a2 = lamed a0 a1 a2

--pattern Rule :: Term' atom -> Term' atom -> Term' atom
--pattern Rule a0 a1 <- TCons (TAtom ARule) (TCons a0 a1)
--  where
--    Rule a0 a1 = drule a0 a1

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
