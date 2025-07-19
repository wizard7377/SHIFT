{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Core.Monad (
  Logic,
  Convert,
  Reduce,
  Rule,
  drive,
  refuel,
  unbind,
  substRaw,
  subst,
  substIn,
  resolve,
  resolveC,
  request,
  runLogic_,
  runLogic,
  addStepRaw,
  addStep,
) where

import Control.Applicative (Alternative (..))
import Control.Lens ((+=))
import Control.Lens.Operators ((%=), (-=), (<>=))
import Control.Monad.Accum qualified as M
import Control.Monad.Identity qualified as M
import Control.Monad.Morph
import Control.Monad.Morph qualified as M
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Control.Monad.Reader qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Writer qualified as M
import Data.Choice
import Data.Foldable (Foldable (..))
import Data.List (delete)
import Data.Text qualified as T
import Extra
import Rift qualified
import Short
import Sift.Core.Types
import Sift.Core.Unify

type Logic e = M.StateT (Local e) (ChoiceT (M.State (Global e)))

type Reduce e = (TOC e) => TO e -> Logic e (TO e)

{- | Conversions in the 'Logic' Monad.
 The first arguement is the primary term
-}
type Convert e = (TOC e) => (TO e) -> (TO e) -> Logic e (TO e)

type Rule e = (TOC e) => Hypothesis e -> Logic e (Hypothesis e)
bullet :: (TOC e) => Logic e (Hypothesis e)
bullet = do
  state <- M.get
  let (goal : rest) = state ^. goals
  goals <>= (rest)
  pure goal
drive :: forall e. Logic e ()
drive = (localDepth += 1)

refuel :: Logic e ()
refuel = (localDepth -= 1)

unbind :: forall e. (TOC e) => TO e -> Logic e ()
unbind t = do
  state <- M.get
  let vars = state ^.. hypos . each . _Bound
  cguard (t `elem` vars)
  (hypos %= (filter notBind))
  pure ()
 where
  notBind (x :: Hypothesis e) = case x of
    Bound y -> not $ t == y
    _ -> True

substRaw :: TO e -> TO e -> Logic e ()
substRaw from to = (hypos <>= (pure $ (Subst :: TO e -> TO e -> Hypothesis e) from to))

subst :: (TOC e) => TO e -> TO e -> Logic e ()
subst from to = unbind from >> substRaw from to

-- | A smart tool for creating subsitutions
substIn ::
  (TOC e) =>
  -- | From
  TO e ->
  -- | To
  TO e ->
  -- | Within
  TO e ->
  -- | Result
  Logic e (TO e)
substIn from to within = do
  -- Add the substitution to the local state
  subst from to
  -- Resolve the within term with the current substitutions
  resolve within

resolve :: (TOC e) => TO e -> Logic e (TO e)
resolve t = do
  state <- M.get
  let binds = state ^.. hypos . each . _Subst
  let binds' = binds <&> (\(x, y) -> ((), x, y))
  let mapping = binds' ^. seeTupMap
  pure $ (mapToF mapping) t

resolveC :: (TOC e) => Hypothesis e -> Logic e (Hypothesis e)
resolveC h = case h of
  Bound x -> Bound <$> resolve x
  Reduce x -> Reduce <$> resolve x
  Equal x y -> Equal <$> resolve x <*> resolve y
  Subst x y -> Subst <$> resolve x <*> resolve y
  Trivial -> pure Trivial
  _ -> pure h
request :: Hypothesis e -> Logic e ()
request h =
  goals <>= (pure h)

runLogic :: (TOC e) => Logic e r -> Rift.LogicEnv -> e -> ([(r, Local e)], Global e)
runLogic f env thy =
  let
    f0 = (M.runStateT f) def
    f1 = runChoiceT f0
    y = M.runIdentity $ (M.runStateT f1) (Global env thy 0)
   in
    y

runLogic_ :: (TOC e) => Logic e r -> Rift.LogicEnv -> e -> [(r, Local e)]
runLogic_ f e = fst . runLogic f e

addStepRaw :: OpTypes -> TO e -> Logic e ()
addStepRaw op arg =
  history %= ((op, arg) :)

addStep :: (TOC e) => OpTypes -> (TO e -> Logic e r) -> TO e -> Logic e r
addStep op f arg = do
  res <- f arg
  addStepRaw op arg
  -- drive
  pure res

convertToRule :: forall e a. (TOC e, Rift.Term (TO e)) => Convert e -> Rule e
convertToRule c g = case g of
  Equal x y -> Equal <$> (c x y) <*> pure y
  Trivial -> pure (Trivial :: Hypothesis e)
  _ -> empty
