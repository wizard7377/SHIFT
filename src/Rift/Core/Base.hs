{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE GHC2021, TemplateHaskell #-}
{-|
Module      : Rift.Core.Base
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}

module Rift.Core.Base (
  KTerm(..),
  AnyTerm,
  Term'(..),
  TestTerm,
  TestToken (..),
  Atomic,
  cons,
  --drule,
  pattern Cons3,
  pattern Lamed,
  pattern Atom,
  --pattern Rule,
  pattern Kaf,
  termToList,
  listToTerm,
  mkCons,
  mkCons3,
  manyLamed,
  TermLike,
  recurseSome,
  recurseManyA,
  recurseManyB,
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
import Extra
import Data.Type.Equality ((:~:) (..))
import qualified Control.Lens as Lens
-- |The type of terms used for testing `Sift`
data Term' atom where
  -- |ל
  PrimLamed :: Term' atom
  -- |ט
  PrimAtom :: atom -> Term' atom
  -- |כ
  PrimCons :: Term' atom -> Term' atom -> Term' atom
  PrimTag :: Term' atom -> Int -> Term' atom
  -- |ד
  PrimFree :: Term' atom -> [Term' atom] -> Term' atom
  -- |`λfrom . λto . λterm . (term[from := to])`
  PrimRep :: Term' atom -> Term' atom -> Term' atom -> Term' atom
  PrimError :: Term' atom
  deriving (Data,Typeable,Generic)

deriving instance Data atom => Plated (Term' atom)
instance (Eq atom, Plated (Term' atom)) => Eq (Term' atom) where
  PrimLamed == PrimLamed = True
  PrimAtom a == PrimAtom b = a == b
  PrimCons a0 a1 == PrimCons b0 b1 = (a0 == b0) && (a1 == b1)
  PrimTag a n == PrimTag b m = (a == b) && (n == m)
  PrimFree a vs == PrimFree b ws = (a == b) && (vs == ws)
  PrimRep a0 a1 a2 == PrimRep b0 b1 b2 =
    (a0 == b0) && (a1 == b1) && (a2 == b2)
  --PrimRep a0 a1 a2 == t = if (occurs a0 a1) then False else ((Lens.transform (change a0 a1) a2) == t)
  --t == PrimRep a0 a1 a2 = if (occurs a0 a1) then False else (t == Lens.transform (change a0 a1) a2)
  PrimError == PrimError = True
  _ == _ = False
instance (KTerm (Term' atom)) where
  isLamed PrimLamed = True
  isLamed _ = False
  mkLamed = PrimLamed
  pKaf = Lens.prism' 
    (\(PrimKafCon a b) -> PrimCons a b)
    (\case
      PrimCons a b -> Just (PrimKafCon a b)
      _ -> Nothing)

type TermLike term = (Eq term, Ord term, Show term)
data TestToken = TestToken (Either Text Int) | TestLogicToken Int
  deriving (Eq, Ord, Data, Generic, Typeable)

type TestTerm = Term' TestToken
{- | The attomic class constraint
Represents a collection of "packaged requirements" that all atoms must have
All of these are fairly standard and should be implemented for most types anyway
-}
type Atomic a = (Eq a, Ord a, Show a)
-- |The functional version of `PrimAtom`, equavilent to `Atom`

{-# DEPRECATED #-}
mkCons3 :: AnyTerm term => term->term->term-> term
mkCons3 f a0 a1 = Kaf f (Kaf a0 a1)

{-# DEPRECATED #-}
mkCons :: AnyTerm term => term->term-> term
mkCons = Kaf
-- |The functional version of `Kaf`, equaivelent to `Kaf`
cons :: AnyTerm term => term->term-> term
cons = Kaf

-- |The functional version of `PrimLamed`, equaivelent to `Lamed`
lamed ::
  KTerm term =>
  -- |The variable
  term ->
  -- |The consequent
  term ->
  -- |The precedent
  term ->
  -- |The result
  term
lamed frees a0 a1 = Kaf BasicLamed (Kaf frees (Kaf a0 a1))

-- FOR TESTS ONLY, THIS IS A PARTIAL FUNCTION
manyLamed :: KTerm term => [term] -> term -> term -> term -> term
manyLamed [t] a0 a1 a2 = Lamed t a0 a1 a2
manyLamed (t : ts) a0 a1 a2 = Lamed t (manyLamed ts a0 a1 a2) a1 a2
--drule = mkCons' ARule


pattern Cons3 :: KTerm term => term -> term -> term -> term
pattern Cons3 f a0 a1 <- Kaf f (Kaf a0 a1)
  where
    Cons3 f a0 a1 = Kaf f (Kaf a0 a1)

pattern Lamed :: KTerm term => term -> term -> term -> term -> term
pattern Lamed v a b f <- Kaf BasicLamed (Kaf v (Kaf a (Kaf b f)))
  where
    Lamed v a b f = Kaf BasicLamed (Kaf v (Kaf a (Kaf b f)))
{-# INLINE Lamed #-}
termToList :: KTerm term => term -> [term]
termToList (Kaf a0 a1) = (:) a0 $ termToList a1
termToList _ = []

listToTerm :: AnyTerm term => [term] -> Maybe (term)
listToTerm [] = Nothing
listToTerm [x] = Just x
listToTerm (x : xs) = Kaf x <$> listToTerm xs

-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseSome :: (Eq term, KTerm term) => (term -> term) -> term -> term
recurseSome f v = let v' = f v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseSome f a0) (recurseSome f a1)
      Atom _ -> v'
      BasicLamed -> BasicLamed
  )
    else v'
-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseManyA :: (Eq term, KTerm term) => (term -> term) -> (term -> term) -> term -> term
recurseManyB :: (Eq term, KTerm term) => (term -> term) -> (term -> term) -> term -> term
recurseManyA f g v = let v' = f v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseManyA f g a0) (recurseManyA f g a1)
      _ -> v'
  )
    else recurseManyB f g v'
recurseManyB f g v = let v' = g v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseManyB f g a0) (recurseManyB f g a1)
      _ -> v'
  )
    else recurseManyA f g v'
-- |Check if a given term occurs within another term
poccurs :: (Eq term, KTerm term) => 
  -- |The larger term
  term -> 
  -- |The smaller term
  term -> Bool 
poccurs term value = 
  (value == term) || (case term of
    Kaf a0 a1 -> (poccurs term a0) || (poccurs term a1)
    Atom _ -> False
    BasicLamed -> False)


