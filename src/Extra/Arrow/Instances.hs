{-# LANGUAGE UndecidableInstances #-}

module Extra.Arrow.Instances where

import Control.Arrow
import Extra.Arrow.Classes
import Extra.Arrow.Reader
import Extra.Arrow.State
import Extra.Arrow.Trans
import Extra.Arrow.Writer

instance (ArrowReader s a, ArrowTrans t a) => ArrowReader s (t a) where
  readArrow = liftArrow readArrow
instance (ArrowWrite w a, ArrowTrans t a) => ArrowWrite w (t a) where
  writeArrow = liftArrow writeArrow
instance (ArrowState w a, ArrowTrans t a) => ArrowState w (t a) where
  fetchArrow = liftArrow fetchArrow
  storeArrow = liftArrow storeArrow
instance (ArrowZero a, ArrowTrans t a) => ArrowZero (t a) where
  zeroArrow = liftArrow zeroArrow

instance (ArrowChoice a, ArrowTrans t a) => ArrowChoice (t a) where
  left arr = left
