module Rift.Forms.Proof where

import Control.Applicative (Alternative (..))
import Extra

data Proof a where
  Given :: a -> Proof a
  Mem :: Proof a -> Proof a -> Proof a
  OneOf :: Proof a -> Proof a -> Proof a
  Reduce :: Proof a -> Proof a
  Failure :: Proof a
  deriving (Eq, Show, Data, Generic, Ord)

instance Semigroup (Proof t) where
  x <> y = Mem x y
