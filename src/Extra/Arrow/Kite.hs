{-# LANGUAGE FunctionalDependencies #-}

module Extra.Arrow.Kite where

import Control.Arrow (Arrow (..), (>>>))
import Control.Category (Category)
import Extra.Basics

{- | The class of all "kites"
 A kite is a generalization of an arrow, where instead of composing two arrows of the same type to yield similar, any of these types may be variant
-}
class Kite (a :: k0 -> k1 -> Type) (b :: k1 -> k2 -> Type) (c :: k0 -> k2 -> Type) | a b -> c where
  -- | Kite composition, in piping order
  kc :: forall (x :: k0) (y :: k1) (z :: k2). a x y -> b y z -> c x z

instance (Category a) => Kite a a a where
  kc = (>>>)

instance (Arrow a) => Kite a (->) a where
  kc f g = f >>> arr g

instance (Arrow a) => Kite (->) a a where
  kc f g = arr f >>> g
