module Rift.Forms.Proof where

import Extra

data Proof a where
  Given :: a -> Proof a
  Mem :: Proof a -> Proof a -> Proof a
  deriving (Eq, Show, Data, Generic, Ord)
