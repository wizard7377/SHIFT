{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Theory
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Theory (
  -- * Inner terms
  -- $termOf
  TermOf,
  Theory (..),
  TheoryOf (..),
  SimpleTheory (..),
  verifyTheory,
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((<>~))
import Data.Data
import Extra
import GHC.Generics
import Rift.Core qualified as Rift

{- $termOf
Terms
-}

{- | The type family for Types of a given `Term`
 In other words, given a type that "uses a given" term, find the term
 This is quite useful, as it allows abstraction over constructs without worrying about the inner terms except when neccasary
-}
type family TermOf t

class (Rift.Term (TermOf t), Rift.Inner (TermOf t) ~ (TermOf t)) => Theory t where
  type LabelOf t

  type LabelOf t = ()
  defines :: Lens' t (TMap (LabelOf t) (TermOf t) (TermOf t))
  defines = Lens.lens getDefines addDefines
  proofs :: Lens' t (TMap (LabelOf t) (TermOf t) (TermOf t))
  proofs = Lens.lens getProofs addProofs

  addDefines :: t -> (TMap (LabelOf t) (TermOf t) (TermOf t)) -> t
  addDefines t img = t & defines <>~ (img)
  getDefines t = Lens.view defines t
  getDefines :: t -> TMap (LabelOf t) (TermOf t) (TermOf t)
  addProofs :: t -> (TMap (LabelOf t) (TermOf t) (TermOf t)) -> t
  addProofs t img = t & proofs <>~ (img)
  getProofs :: t -> TMap (LabelOf t) (TermOf t) (TermOf t)
  getProofs t = Lens.view proofs t
  {-# MINIMAL (defines, proofs) | (addDefines, getDefines, addProofs, getProofs) #-}

-- | First term, then theory
class (Theory e, TermOf e ~ t) => TheoryOf t e | e -> t

data SimpleTheory inf tok term = SimpleTheory
  { _defines :: TMap tok term term
  , _proofs :: TMap tok term term
  , _symbols :: Map tok inf
  }
  deriving (Eq, Ord, Data, Typeable, Generic)

deriving instance (Show inf, Show term, Show tok) => Show (SimpleTheory inf tok term)
type instance TermOf (SimpleTheory inf tok term) = term
instance (Rift.Term term) => Theory (SimpleTheory inf tok term) where
  type LabelOf (SimpleTheory inf tok term) = tok
  defines = Lens.lens _defines (\t img -> t{_defines = img})
  proofs = Lens.lens _proofs (\t img -> t{_proofs = img})

verifyTheory :: (Theory t, Rift.Term (TermOf t)) => (t -> TermOf t -> TermOf t -> r) -> t -> [r]
verifyTheory f t =
  let defs = getDefines t
      proofs = getProofs t
   in (\(_, lhs, rhs) -> f t lhs rhs) <$> (proofs ^. seeMapTup)

instance (Theory e, TermOf e ~ t) => TheoryOf t e

instance Default (SimpleTheory inf tok term) where
  def = SimpleTheory mempty mempty mempty
