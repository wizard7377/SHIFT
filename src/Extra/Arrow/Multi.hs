{-# LANGUAGE Arrows #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE LambdaCase #-}

module Extra.Arrow.Multi (MultiArrow (..), type (+>)) where

import Control.Arrow (Arrow (..), (>>>))
import Control.Arrow qualified as A
import Control.Category (Category (id, (.)))
import Control.Category qualified as C
import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Identity qualified as F
import Data.List (singleton)
import Prelude hiding (id, (.))

-- | The multi-arrow, a non-deterministic computation
newtype MultiArrowT t a b = MultiArrow {runMultiArrow :: t a [b]}

type MultiArrow = MultiArrowT (->)
infixr 0 +>
type a +> b = MultiArrow a b

instance (Arrow a) => C.Category (MultiArrowT a) where
  id :: (Arrow a) => MultiArrowT a x x
  id = MultiArrow $ arr singleton . id
  (.) :: (Arrow a) => MultiArrowT a b c -> MultiArrowT a x b -> MultiArrowT a x c
  (MultiArrow f) . (MultiArrow g) = proc x -> do
    r0 <- g -< x
    case r0 of
      (y : ys) -> do
        z <- f -< y
        zs <- f -< ys
        A.returnA -< z : zs
      [] -> A.returnA -< []
