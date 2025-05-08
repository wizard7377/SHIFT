{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Kernel (
  PrimTerm (..),
  TermLike (..),
) where

import Data.Data
import Data.Text (Text)
import Extra
import GHC.Generics

type family Fundemental :: k -> Type

-- | The class of all top level sentences, which can convert to @Term@ tok
class TermLike atom term | term -> atom where
  toTerm :: term -> PrimTerm atom

{- | The basic primitive types of terms, with only 3 basic constructors
 Note that ל is not given as an atom, merely for the sake of effeciancy
 Also note that the ל construct is not explicitly given, it is defined purely as any expression of the form (ל . α . β . γ) where "." is `PrimCons`.
 In this instance, α is the qualification, β is the head (result) and γ is the body (precedent).
 This is roughly equivalent to \( \Gamma \alpha (\gamma \arrow \beta) \)
-}
data PrimTerm atom where
  -- | The ל symbol, as documented above
  PrimLamed :: PrimTerm atom
  -- | The form of all other atoms, ת
  PrimAtom :: atom -> PrimTerm atom
  -- | The cons constructor, כ
  PrimCons :: PrimTerm atom -> PrimTerm atom -> PrimTerm atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord, Eq)
