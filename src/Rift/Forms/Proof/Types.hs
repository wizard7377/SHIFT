{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Rift.Forms.Proof.Types where

import Rift.Forms.Proof.Abstract

instance (Eq a) => Proof a where
  type TermOf a = a
  getScore spec proof = if spec == proof then Solved else Chance 0.5
