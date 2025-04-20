{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Rift.Core.Base (Term (..), Sentence (..), TestTerm, Atomic) where

import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Type)
import Data.Text as T
import GHC.Generics (Generic)
import Text.Show.Functions ()

data GenericTerm cons atom where
  Atom :: atom -> GenericTerm atom
  Yud :: GenericTerm atom
  Cons :: cons -> GenericTerm atom -> GenericTerm atom -> GenericTerm atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord)
data BaseCons = Lamed | Cons | Rule

{- | The type of all abstract terms in SHIFt
 Note that this definition is quite minimal, concrete implementation details are hidden from difference parts of the compiler by the @atom@ parameter
-}
data Term atom where
  -- | Atomic values, that is, primitives
  Atom :: atom -> Term atom
  -- | The empty list, \(()\)
  Empty :: Term atom
  -- | The yud, or universe type
  Yud :: Term atom
  -- | The Lamed, or quantified, type
  Lamed :: Term atom -> Term atom -> Term atom
  -- | The cons type, that is, @x . y@
  Cons :: Term atom -> Term atom -> Term atom
  -- | The subtyping type, that is @a : b@
  Rule :: Term atom -> Term atom -> Term atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord)

deriving instance (Eq atom) => Eq (Term atom)
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
