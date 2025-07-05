{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Mem (memReduce) where

import Control.Applicative (Alternative (..))
import Control.Lens (transform)
import Control.Monad.Morph
import Control.Monad.Trans
import Data.Foldable (toList)
import Extra
import Extra.Choice.Combinators (recover)
import Rift qualified
import Sift.Core.Unify
import Sift.Ops.Common
import Sift.Search.Convert
import {-# SOURCE #-} Sift.Search.ReducePrime

{- | The mem reduce function performs mem reduction
-- That is, it takes a term of the form:
-- @[α*]([β*](לγδεζ)[η*]θ)@
-- α*, β*, η* are lists of terms, γ, δ, ε, ζ, and θ are terms (and, as a pracitcal limitation, γ is a name)
--
-- The semantics are as follows:
-- Consider the unification of
-}
memReduce :: Redux
memReduce (Rift.FreeTerm freeBothv (Rift.Kaf (Rift.FreeTerm freeLeftv (Rift.Lamed var bad arg ans)) input)) = recover (pure $ Rift.FreeTerm freeBothv (Rift.Kaf (Rift.FreeTerm freeLeftv bad) input)) $ do
  uni <- convert freeBothv (arg & Rift.frees <>~ arg : freeLeftv) input
  let ans' = applyRes uni ans
  pure ans'
memReduce _ = empty
