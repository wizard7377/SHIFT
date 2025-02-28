{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}

module Rift.Base (Term (..), Sentence (..), TestTerm, Atomic) where

import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Type)
import Data.Text as T
import GHC.Generics (Generic)
import Text.Show.Functions ()

data Term atom where
  Atom :: atom -> Term atom
  Empty :: Term atom
  Yud :: Term atom
  Lamed :: Term atom -> Term atom -> Term atom
  Cons :: Term atom -> Term atom -> Term atom
  Rule :: Term atom -> Term atom -> Term atom
  deriving (Functor, Foldable, Traversable, Data, Generic)

type TestTerm = Term Text

type Atomic a = (Eq a)

-- type Atom a = (Eq a, Show a)

-- | The class of all top level sentences, which can convert to `Term' tok`
class Sentence sen where
  fromTerm :: Term atom -> sen atom
  toTerm :: sen atom -> Term atom

instance Sentence Term where
  fromTerm = id
  toTerm = id
