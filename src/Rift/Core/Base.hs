{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Rift.Core.Base (GenericTerm (..), BaseCons (..), Term (..), Sentence (..), TestTerm, Atomic, lamed, cons, rule) where

import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()

data GenericTerm cons atom where
  Atom :: atom -> GenericTerm cons atom
  Yud :: GenericTerm cons atom
  BCons :: cons -> GenericTerm cons atom -> GenericTerm cons atom -> GenericTerm cons atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord, Eq)
data BaseCons = Lamed | Cons | Rule
  deriving (Data, Generic, Ord, Eq)

{- | The type of all abstract terms in SHIFt
 Note that this definition is quite minimal, concrete implementation details are hidden from difference parts of the compiler by the @atom@ parameter
-}
type Term = GenericTerm BaseCons

type TestTerm = Term Text

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

lamed :: Term atom -> Term atom -> Term atom
lamed = BCons Lamed
cons :: Term atom -> Term atom -> Term atom
cons = BCons Cons
rule :: Term atom -> Term atom -> Term atom
rule = BCons Rule
