module Extra.Types where

import Data.Kind

flip :: forall k0 k1 k2. (k0 -> k1 -> k2) -> k1 -> k0 -> k2
flip t b a = t a b
