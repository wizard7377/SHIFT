{-# LANGUAGE FunctionalDependencies #-}

module Extra.Arrow.Classes where

import Control.Arrow
import Control.Arrow qualified as A
import Data.Bifunctor (Bifunctor (..))
import Data.List (singleton)

class ArrowReader r a | a -> r where
  readArrow :: a b r
  {-# MINIMAL readArrow #-}
class ArrowState s a | a -> s where
  fetchArrow :: a b s
  storeArrow :: a s ()
  {-# MINIMAL fetchArrow, storeArrow #-}
class (Monoid w) => ArrowWrite w a | a -> w where
  writeArrow :: a w ()
  {-# MINIMAL writeArrow #-}

class ArrowMulti a where
  arrL :: (b -> [c]) -> a b c
  mapL :: ([c] -> [d]) -> a b c -> a b d
