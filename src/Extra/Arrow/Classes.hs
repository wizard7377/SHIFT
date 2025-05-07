{-# LANGUAGE FunctionalDependencies #-}

module Extra.Arrow.Classes where

import Control.Arrow
import Data.Bifunctor (Bifunctor (..))

class (Arrow a) => ArrowReader r a | a -> r where
  readArrow :: a b r
  {-# MINIMAL readArrow #-}
class (Arrow a) => ArrowState s a | a -> s where
  fetchArrow :: a b s
  storeArrow :: a s ()
  {-# MINIMAL fetchArrow, storeArrow #-}
class (Arrow a, Monoid w) => ArrowWrite w a | a -> w where
  writeArrow :: a w ()
  {-# MINIMAL writeArrow #-}

peeking :: (ArrowReader r a) => a (b, r) c -> a b c
peeking = _

stateful :: (ArrowState s a) => a (b, s) (c, s) -> a b c
stateful = _
