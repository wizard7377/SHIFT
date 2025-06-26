{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Extra.Choice.Combinators
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Extra.Choice.Combinators where

import Control.Applicative (Alternative (..))
import Extra.Choice.Types (MonadChoice (..))

cguard :: (Alternative m) => Bool -> m ()
cguard True = pure ()
cguard False = empty
cguardM :: (Monad m, Alternative m) => m Bool -> m ()
cguardM m = do
  b <- m
  cguard b
csingle :: (MonadChoice m, Alternative m) => m a -> m a
csingle m = do
  choices <- cget m
  case choices of
    Nothing -> empty
    Just (x, _) -> pure x

-- | Use a default value if there are no choices
recover ::
  (Monad m, MonadChoice m, Alternative m) =>
  -- | The input
  m a ->
  -- | The default
  m a ->
  -- | The output
  m a
recover m v = do
  xs <- cget v
  case xs of
    Nothing -> csingle m
    Just _ -> v

cnot :: (MonadChoice m, Alternative m) => m a -> m ()
cnot m = do
  choices <- cget m
  case choices of
    Nothing -> pure ()
    Just _ -> empty

-- | Technically not a choice combinator
coptional :: (Alternative m, Monad m) => (a -> m a) -> a -> m a
coptional f m = pure m <|> f m

-- Returns the cross product of the two choices, ie, @(C := A & B) => ∀a∀b.((a ∈ A), (b ∈ B) <=> (a * b) ∈ C)@
cand :: (Monad m, MonadChoice m, Alternative m) => m a -> m b -> m (a, b)
cand m1 m2 = do
  x1 <- cget m1
  x2 <- cget m2
  case (x1, x2) of
    (Just (a, al), Just (b, bl)) -> (pure (a, b)) <|> ((a,) <$> bl) <|> ((,b) <$> al) <|> (cand al bl)
    _ -> empty
cor :: (Alternative m) => m a -> m a -> m a
cor = (<|>)

-- | Choice if then else
cifte ::
  (Monad m, MonadChoice m) =>
  -- | Condition
  m a ->
  -- | Then
  m b ->
  -- | Else
  m b ->
  m b
cifte cond ifTrue ifFalse = do
  choices <- cget cond
  case choices of
    Just (_, _) -> ifTrue
    Nothing -> ifFalse
