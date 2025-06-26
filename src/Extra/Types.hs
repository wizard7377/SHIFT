{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.Types where

import Data.Data
import Data.Kind
import Data.Word (Word64)
import GHC.Generics (Generic)

flip :: forall k0 k1 k2. (k0 -> k1 -> k2) -> k1 -> k0 -> k2
flip t b a = t a b

class (Data a) => Functional a b c | a b -> c where
  fapply :: a -> b -> c

type Idx = Int
newtype Generator = Generator Idx
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

getNumber :: Generator -> (Idx, Generator)
getNumber (Generator n) = (n, Generator (n + 1))

-- | The typeclass synonym for which all "concrete" types should satisfy (ie, all things that are not magic, like `->` or `IO`)
type Concrete a = (Show a, Eq a, Ord a, Data a, Typeable a, Generic a)
