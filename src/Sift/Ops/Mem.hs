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
  binds <- hoist lift $ unify freeBothv (Rift.addFrees arg (var : freeLeftv)) input
  bindMap <- fromList $ toList $ normalMap (binds ^. unifyGraph)
  let frees = (binds ^. freeLeft) <> (binds ^. freeRight) <> (binds ^. freeBoth)
  let newTerm = foldr (\(_, k, v) -> Rift.replaceTerm k v) ans (bindMap ^. seeMapTup)
  pure (Rift.addFrees newTerm frees)
memReduce _ = empty
