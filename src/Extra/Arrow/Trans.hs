{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Arrow.Trans where

import Control.Arrow (Arrow (..))
import Control.Arrow as A
import Control.Category qualified as C

class (Arrow a, Arrow (t a)) => ArrowTrans t a where
  -- | Lift an arrow into the transformer
  liftArrow :: (Arrow a) => a b c -> t a b c

instance (C.Category (t a), ArrowTrans t a) => Arrow (t a)
