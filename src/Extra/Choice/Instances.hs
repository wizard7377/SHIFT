{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.Choice.Instances where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.List (singleton)
import Data.List.Extra (nubBy)
import Data.Maybe
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra.Basics
import Extra.Choice.Core
import Extra.Choice.Types
import Extra.List
import Extra.Ops

instance (Functor m) => Functor (ChoiceT m) where
  fmap f (ChoiceT m) = ChoiceT $ fmap (fmap f) m

instance (Monad m) => Applicative (ChoiceT m) where
  pure x = ChoiceT $ pure $ MCons x (pure MNil)
  ChoiceT f <*> ChoiceT xs = ChoiceT $ do
    fs <- f
    xs' <- xs
    return $ fs <*> xs'

-- Why on earth isn't Monad declared `class Functor m => Monad m'?
-- I assume that a monad is always a functor, so the contexts
-- get a little larger than actually necessary
instance (Monad m) => Monad (ChoiceT m) where
  m >>= f = joinListT $ fmap f m

instance MonadTrans ChoiceT where
  lift = ChoiceT . fmap (`MCons` return MNil)

instance (Monad m) => Alternative (ChoiceT m) where
  empty = ChoiceT $ return MNil
  ChoiceT xs <|> ChoiceT ys = ChoiceT $ mAppend xs ys

instance (Monad m, Semigroup a) => Semigroup (ChoiceT m a) where
  xs <> ys = (<>) <$> xs <*> ys

instance (Monad m, Monoid a) => Monoid (ChoiceT m a) where
  mempty = ChoiceT $ return MNil
  mappend = (<>)

instance (Monad m) => MonadPlus (ChoiceT m) where
  mzero = empty
  mplus = (<|>)

foldMapStep :: (Foldable m, Monad m, Monoid b) => (a -> b) -> StepT m a -> m b
foldMapStep f (MCons x xs) = (<>) (f x) <$> (foldMapStep f <| xs)
foldMapStep _ MNil = return mempty
foldMapList :: (Foldable m, Monad m, Monoid b) => (a -> b) -> ListT m a -> m b
foldMapList f l = foldMapStep f <| l
traverseStep :: (Traversable m, Monad m, Applicative f) => (a -> f b) -> StepT m a -> f (StepT m b)
traverseStep f (MCons x xs) =
  let
    y0 = f x
    y1 = traverseList f xs
   in
    MCons <$> y0 <*> y1
traverseStep f MNil = pure MNil
traverseList :: (Traversable m, Monad m, Applicative f) => (a -> f b) -> ListT m a -> f (ListT m b)
traverseList f l = traverse (traverseStep f) l

traverseChoiceT :: (Traversable m, Monad m, Applicative f) => (a -> f b) -> ChoiceT m a -> f (ChoiceT m b)
traverseChoiceT f (ChoiceT m) = ChoiceT <$> traverseList f m
instance (Foldable m, Monad m) => Foldable (ChoiceT m) where
  foldMap :: (Foldable m, Monad m, Monoid t) => (a -> t) -> ChoiceT m a -> t
  foldMap f (ChoiceT m) = fold $ foldMapList f m

instance (Traversable m, Monad m) => Traversable (ChoiceT m) where
  traverse f (ChoiceT m) = traverseChoiceT f (ChoiceT m)

instance {-# OVERLAPPABLE #-} (Monad m) => MonadFail (ChoiceT m) where
  fail _ = empty
instance (Monad m) => MonadChoice (ChoiceT m) where
  cset :: (Monad m) => Yield (ChoiceT m) a -> ChoiceT m a
  cset input = _

  cget :: (Monad m) => ChoiceT m a -> Yield (ChoiceT m) a
  cget = _
