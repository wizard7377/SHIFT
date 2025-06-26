{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Theory
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Theory where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((<>~))
import Data.Data
import Extra
import GHC.Generics
import Rift.Core qualified as Rift

class (Rift.Term (TermOf t), Rift.Inner (TermOf t) ~ (TermOf t)) => Theory t where
  type TermOf t
  type LabelOf t
  type LabelOf t = ()
  defines :: Lens' t (TMap (LabelOf t) (TermOf t) (TermOf t))
  defines = Lens.lens getDefines addDefines
  proofs :: Lens' t (TMap (LabelOf t) (TermOf t) (TermOf t))
  proofs = Lens.lens getProofs addProofs
  addDefines :: t -> (TMap (LabelOf t) (TermOf t) (TermOf t)) -> t
  addDefines t img = t & defines <>~ (img)
  getDefines t = Lens.view defines t
  getDefines :: t -> TMap (LabelOf t) (TermOf t) (TermOf t)
  addProofs :: t -> (TMap (LabelOf t) (TermOf t) (TermOf t)) -> t
  addProofs t img = t & proofs <>~ (img)
  getProofs :: t -> TMap (LabelOf t) (TermOf t) (TermOf t)
  getProofs t = Lens.view proofs t
  {-# MINIMAL (defines, proofs) | (addDefines, getDefines, addProofs, getProofs) #-}
