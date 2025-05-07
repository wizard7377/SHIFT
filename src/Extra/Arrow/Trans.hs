module Extra.Arrow.Trans where

import Control.Arrow (Arrow (..))
import Control.Arrow as A

class ArrowTrans t where
  -- | Lift an arrow into the transformer
  liftArrow :: (Arrow a) => a b c -> t a b c
