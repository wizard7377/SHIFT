module Rift.Core.Kernel where

import Data.Data
import Data.Text (Text)
import GHC.Generics

data Term atom where
  TAtom :: atom -> Term atom
  TCons :: Term atom -> Term atom -> Term atom
  deriving (Functor, Foldable, Traversable, Data, Generic, Ord, Eq)

data BasicAtom atom = ALamed | ARule | AAtom atom
  deriving (Data, Generic, Ord, Eq)

instance (Show atom) => Show (BasicAtom atom) where
  show (AAtom a) = show a
  show ALamed = "?"
  show ARule = ":"
