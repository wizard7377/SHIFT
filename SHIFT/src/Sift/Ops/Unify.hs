{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Ops.Unify
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Ops.Unify where

import Control.Lens.Operators ((<>=))
import Control.Monad.Morph
import Rift qualified
import Short
import Sift.Core.Monad
import Sift.Core.Types
import Sift.Core.Unify

unifyConvert :: (TOC e) => Convert e
unifyConvert t0 t1 = do
  r0 <- lift $ hoist lift $ unify' t0 t1
  let r1 = uniToRes r0
  hypos <>= uniToHypo r1
  pure t1
