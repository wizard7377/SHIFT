{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}

module Rift.Core.Kernel (
  KTerm,
  {-# WARNING "This should only be used in the implementation" #-}
  PrimKaf(..),
  {-# WARNING "This should only be used in the implementation" #-}
  KTerm(..),
  pattern Atom,
  pattern Cons,
  pattern BasicLamed,
  pattern Kaf,
  AnyTerm,
) where

import Control.Lens (Plated (..), Prism', preview, review, traversal, (^?))
import Control.Lens.Extras (is)
import Data.Data
import Data.Text (Text)
import Extra
import GHC.Generics

type family Fundemental :: k -> Type

data PrimKaf term = PrimKafCon term term

{- | The basic class of terms
 - Fundementally, each term is something that has a way to plate to and from a nullary ל constructor, a unary ת constructor, and binary כ constructor
 -
-}
class KTerm term where
  pKaf :: Prism' term (PrimKaf term)
  isLamed :: term -> Bool
  mkLamed :: term

type AnyTerm term = KTerm term

-- | Simply a case where we rewrite over `Cons`
instance (KTerm term) => Plated term where
  plate f x =
    case x ^? pKaf of
      Just (PrimKafCon a b) -> review pKaf <$> (PrimKafCon <$> f a <*> f b)
      Nothing -> pure x

-- | The pattern synonym for atom
pattern Atom' :: (KTerm term) => term
pattern Atom' <- (is pKaf -> False)

pattern Atom :: (KTerm term) => term -> term
pattern Atom atom <- atom@Atom'

-- | The pattern sysnonym for cons
{-# DEPRECATED Cons "Use Kaf instead" #-}
pattern Cons :: (KTerm term) => term -> term -> term
pattern Cons t0 t1 = Kaf t0 t1

pattern Kaf :: (KTerm term) => term -> term -> term
pattern Kaf t0 t1 <- (preview pKaf -> Just (PrimKafCon t0 t1))
  where
    Kaf t0 t1 = review pKaf (PrimKafCon t0 t1)

-- | The pattern synoynm for lamed
pattern BasicLamed :: (KTerm term) => term
pattern BasicLamed <- (isLamed -> True)
  where
    BasicLamed = mkLamed

{-# INLINE Cons #-}
{-# INLINE BasicLamed #-}
{-# INLINE Atom #-}
