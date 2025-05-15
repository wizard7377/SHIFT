{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.Proof.Abstract where

data ProofScore = Solved | Chance Float | Invalid
class Proof s where
  type TermOf s
  getScore :: s -> TermOf s -> ProofScore
