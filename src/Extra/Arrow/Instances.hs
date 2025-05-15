{-# LANGUAGE Arrows #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Arrow.Instances where

import Control.Arrow
import Control.Arrow qualified as A
import Control.Category qualified as C
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

instance (Arrow a) => ArrowReader r (ReaderArrowT r a) where
  readArrow :: ReaderArrowT r a b r
  readArrow = ReaderArrow $ A.arr snd

instance (Arrow a, Monoid w) => (ArrowWrite w (WriterArrowT w a)) where
  writeArrow :: WriterArrowT w a w ()
  writeArrow = WriterArrow $ A.arr ((),)

instance (Arrow a) => (ArrowState s (StateArrowT s a)) where
  fetchArrow :: (Arrow a) => StateArrowT s a b s
  fetchArrow = StateArrow $ A.arr (\(_, s) -> (s, s))
  storeArrow :: (Arrow a) => StateArrowT s a s ()
  storeArrow = StateArrow $ A.arr (\(b, _) -> ((), b))
instance (Arrow a, C.Category (StateArrowT s a)) => ArrowTrans (StateArrowT s) a where
  liftArrow :: (Arrow a, Arrow a) => a b c -> StateArrowT s a b c
  liftArrow = _
mapA :: (ArrowChoice cat) => cat a1 a2 -> cat [a1] [a2]
mapA f = arr listCase >>> arr (const []) ||| (f *** mapA f >>> arr (uncurry (:)))
listCase [] = Left ()
listCase (x : xs) = Right (x, xs)

instance (ArrowZero a, ArrowTrans t a) => ArrowZero (t a) where
  zeroArrow = liftArrow zeroArrow
