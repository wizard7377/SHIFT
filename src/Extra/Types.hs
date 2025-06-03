{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.Types where

import Data.Data
import Data.Kind

flip :: forall k0 k1 k2. (k0 -> k1 -> k2) -> k1 -> k0 -> k2
flip t b a = t a b

class (Data a) => Functional a b c | a b -> c where
  fapply :: a -> b -> c

type Idx = Int
