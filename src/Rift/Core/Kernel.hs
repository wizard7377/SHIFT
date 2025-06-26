{-# OPTIONS_HADDOCK show-extensions, prune #-}
{-# LANGUAGE GHC2021, TemplateHaskell #-}
{-|
Module      : Rift.Core.Kernel
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}

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
  pattern Atom',
  pattern Cons,
  pattern BasicLamed,
  pattern Kaf,
  AnyTerm,
  occurs,
) where

import Control.Lens (Plated (..), Prism', preview, review, traversal, (^?))
import Control.Lens.Extras (is)
import Data.Data
import Data.Text (Text)
import Extra
import GHC.Generics
import Rift.Core.Classes
import qualified Control.Lens as Lens

type family Fundemental :: k -> Type


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
{-# COMPLETE BasicLamed, Atom', Kaf #-}
-- |Seperate to allow for `X =>>= X`
occurs :: (Eq term, KTerm term) => term -> term -> Bool
occurs t0 t1 = if t0 == t1 then False else occurs' t0 t1
occurs' :: (Eq term, KTerm term) => term -> term -> Bool
occurs' t0 t1 | t0 == t1 = True
occurs' t0 (Kaf a b) = occurs' t0 a || occurs' t0 b
occurs' t0 _ | otherwise = False
