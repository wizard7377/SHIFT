{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.ChoiceOld where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Extra (mapMaybeM)
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Debug.Trace (trace, traceId, traceShowId)
import Extra.Basics
import Extra.Debug
import Extra.List
import Extra.Ops

class (MonadPlus m, Functor m, Applicative m, Monad m) => MonadChoice (m :: Type -> Type)

{- | The model of choice
- This is the monad transformer version.
- Given an inner monad `m`, and a value `a`, returns a type
-}
newtype ChoiceTT (a :: Type -> Type) (b :: Type -> Type) (c :: Type) = Choice (a [b c])

type ChoiceT = ChoiceTT Identity
type Choice = ChoiceT Identity

instance (Show (n a), Comonad m) => Show (ChoiceTT m n a) where
  show (Choice xs) = "Choice " ++ show (extract xs)
deriving instance (Eq (m [n a])) => Eq (ChoiceTT m n a)

instance (Functor f, Functor g) => Functor (ChoiceTT f g) where
  fmap f (Choice xs) = Choice $ fmap (fmap $ fmap f) xs

instance (Applicative f, Applicative g) => Applicative (ChoiceTT f g) where
  pure x = Choice $ pure [pure x]
  (Choice fs) <*> (Choice xs) = Choice $ liftA2 (<*>) <$> fs <*> xs

instance (Applicative a, Applicative b) => Alternative (ChoiceTT a b) where
  empty = Choice $ pure []
  (Choice xs) <|> (Choice ys) = Choice $ (++) <$> xs <*> ys

instance (Applicative m, Monoid (m a), Monad m) => MonadPlus (ChoiceT m) where
  mzero = Choice $ pure []
  mplus (Choice xs) (Choice ys) = Choice $ (++) <$> xs <*> ys

instance (Monad n) => Monad (ChoiceT n) where
  xs >>= f = choiceBind xs f

choiceBind :: (Monad n, Functor n, Applicative n) => ChoiceT n a -> (a -> ChoiceT n b) -> ChoiceT n b
choiceBind !xs !f =
  let
    !r0 = f <| xs
   in
    r0
instance (Monad a, Alternative a, Functor a, Foldable a) => MonadTrans (ChoiceT) where
  lift :: (Monad m) => m v -> ChoiceT m v
  lift x = Choice $ pure ((: []) x)

instance (Semigroup a, Applicative m) => Semigroup (ChoiceT m a) where
  Choice xs <> Choice ys = Choice $ (<>) <$> xs <*> ys

instance (Monoid a, Applicative m) => Monoid (ChoiceT m a) where
  mempty = Choice $ pure []

-- instance (Traversable a, Traversable b) => Traversable (ChoiceTT a b) where
--  traverse f (Choice xs) = Choice (f <| xs)

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

choice :: (Applicative m, Applicative n) => [a] -> ChoiceTT m n a
choice input = Choice $ pure ([] <> (pure <$> input))

capply :: (Functor m) => (a -> b) -> ChoiceT m a -> ChoiceT m b
capply f (Choice as) = Choice $ f <$$$> as

-- TODO TODO TODO
cfilterMap :: (Functor m) => (n a -> (Maybe (n b))) -> ChoiceTT m n a -> ChoiceTT m n b
cfilterMap f (Choice as) = Choice $ mapMaybe f <$> as
cfilter :: (Functor m, Monad m) => (n a -> Bool) -> ChoiceTT m n a -> ChoiceTT m n a
cfilter f (Choice as) =
  let
    ff = (\x -> if f x then Just x else Nothing)
   in
    cfilterMap ff (Choice as)

-- cfilter f (Choice as) = choice $ filter f as

(<&&>) :: (Semigroup a, Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
(<||>) :: (Applicative m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
(<&&>) = (<>)

(<||>) = choiceOr

infixl 9 <&&>

infixl 8 <||>

solve :: (Functor m, Monad m) => (n a -> Bool) -> ChoiceTT m n a -> ChoiceTT m n a
solve = cfilter

runChoiceT :: ChoiceTT m n a -> m [n a]
runChoiceT (Choice xs) = xs

resolve' :: ChoiceTT m n a -> m [n a]
resolve' (Choice choice) = choice

resolve :: (Comonad m, Comonad n) => ChoiceTT m n a -> [a]
resolve (Choice choice) = extract <$> extract choice
