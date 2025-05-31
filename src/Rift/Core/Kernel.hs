{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Core.Kernel (
  Term (..),
  pattern Atom,
  pattern Cons,
  pattern BasicLamed,
  {-# WARNING "Should not be used outside of implementations" #-}
  PrimTermCon (..),
  AnyTerm,
) where

import Data.Data
import Data.Text (Text)
import Extra
import GHC.Generics
import Control.Lens (Plated(..))

type family Fundemental :: k -> Type
data PrimTermCon term atom where
  PrimLamedCon :: PrimTermCon term atom
  PrimAtomCon :: atom -> PrimTermCon term atom
  PrimConsCon :: term -> term -> PrimTermCon term atom
  deriving (Eq, Ord)

{-| The basic class of terms 
 - Fundementally, each term is something that has a way to plate to and from a nullary ל constructor, a unary ת constructor, and binary כ constructor
 - -}
class Term term where
  type AtomOf term :: Type
  viewTerm :: term -> PrimTermCon term (AtomOf term)
  makeTerm :: PrimTermCon term (AtomOf term) -> term

type AnyTerm term = Term term

-- |Simply a case where we rewrite over `Cons`
instance (Term term) => Plated term where 
  plate f (Cons a b) = Cons <$> f a <*> f b
-- | The pattern synonym for atom
pattern Atom :: (Term term) => AtomOf term -> term
pattern Atom a0 <- (viewTerm -> PrimAtomCon a0)
  where
    Atom a0 = makeTerm (PrimAtomCon a0)

-- | The pattern sysnonym for cons
pattern Cons :: (Term term) => term -> term -> term
pattern Cons t0 t1 <- (viewTerm -> PrimConsCon t0 t1)
  where
    Cons a0 a1 = makeTerm (PrimConsCon a0 a1)
-- | The pattern synoynm for lamed
pattern BasicLamed :: (Term term) => term
pattern BasicLamed <- (viewTerm -> PrimLamedCon)
  where
    BasicLamed = makeTerm PrimLamedCon

{-# INLINE Cons #-} 
{-# INLINE BasicLamed #-} 
{-# INLINE Atom #-} 
