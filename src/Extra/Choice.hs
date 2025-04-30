{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Choice where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra.Basics
import Extra.List
import Extra.Ops

class (MonadPlus m, Functor m, Applicative m, Monad m) => MonadChoice (m :: Type -> Type)

{- | The model of choice
- This is the monad transformer version.
- Given an inner monad `m`, and a value `a`, returns a type
-}
newtype ChoiceT (m :: Type -> Type) (a :: Type) = Choice (m ([] a))

type Choice = ChoiceT Identity

deriving instance (Show (m [a])) => Show (ChoiceT m a)

deriving instance (Eq (m [a])) => Eq (ChoiceT m a)

instance (Functor f) => Functor (ChoiceT f) where
  fmap f (Choice xs) = Choice $ fmap (fmap f) xs

instance (Applicative f) => Applicative (ChoiceT f) where
  pure x = Choice $ pure [x]
  (Choice fs) <*> (Choice xs) = Choice $ (<*>) <$> fs <*> xs

instance (Applicative m) => Alternative (ChoiceT m) where
  empty = Choice $ pure []
  (Choice xs) <|> (Choice ys) = Choice $ (++) <$> xs <*> ys

instance (Foldable m, Applicative m) => Monad (ChoiceT m) where
  return = pure
  x >>= f =
    let Choice choices = f <$> x
        choices1 = foldl (<|>) empty choices
        choices2 = foldl (<|>) empty choices1
     in choices2

instance (forall m. (Monad m) => Monad (ChoiceT m)) => MonadTrans ChoiceT where
  lift :: (Monad m) => m a -> ChoiceT m a
  lift x = Choice $ (: []) <$> x

instance (Semigroup a, Applicative m) => Semigroup (ChoiceT m a) where
  Choice xs <> Choice ys = Choice $ (<>) <$> xs <*> ys

instance (Monoid a, Applicative m) => Monoid (ChoiceT m a) where
  mempty = Choice $ pure [mempty]
  mappend xs ys = xs <> ys

instance (Foldable f) => Foldable (ChoiceT f) where
  foldMap :: (Monoid m) => (a -> m) -> ChoiceT f a -> m
  foldMap f (Choice xs) = foldMap (foldMap f) xs

instance (Functor m, Traversable m) => Traversable (ChoiceT m) where
  traverse f (Choice xs) = Choice <$> traverse (traverse f) xs

-- instance Traversable Choice where

-- | The simple choice, that is, (A | F)
simple :: (Applicative m) => a -> ChoiceT m a
simple = pure

trivial :: (Monoid (m [a])) => ChoiceT m a
trivial = Choice mempty

cabsurd :: (Applicative m) => ChoiceT m a
cabsurd = Choice $ pure []

choiceAnd :: (Semigroup a, Semigroup (m a), Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
choiceAnd = (<>)

choiceOr :: (Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
choiceOr = (<|>)

choice :: (Applicative m) => [a] -> ChoiceT m a
choice input = Choice $ pure ((++ []) input)

capply :: (Functor m) => (a -> b) -> ChoiceT m a -> ChoiceT m b
capply f (Choice as) = Choice $ f <$$> as

cfilterMap :: (Functor m) => (a -> Maybe b) -> ChoiceT m a -> ChoiceT m b
cfilterMap f c =
  let (Choice r0) = capply f c
      r1 = mapMaybe id <$> r0
   in Choice r1

cfilter :: (Functor m) => (a -> Bool) -> ChoiceT m a -> ChoiceT m a
cfilter prop c =
  let propc = \x -> if prop x then Just x else Nothing
      (Choice r0) = capply propc c
      r1 = mapMaybe id <$> r0
   in Choice r1

-- cfilter f (Choice as) = choice $ filter f as

(<&&>) :: (Semigroup a, Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
(<||>) :: (Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
(<&&>) = (<>)

(<||>) = choiceOr

infixl 9 <&&>

infixl 8 <||>

solve :: (Functor m) => (a -> Bool) -> ChoiceT m a -> ChoiceT m a
solve = cfilter

runChoiceT :: ChoiceT m a -> m [a]
runChoiceT (Choice xs) = xs

resolve' :: ChoiceT m a -> m [a]
resolve' (Choice choice) = choice

resolve :: (Comonad m) => ChoiceT m a -> [a]
resolve (Choice choice) = extract choice
