{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.ChoiceNew where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.List (singleton)
import Data.List.Extra (nubBy)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra ((<&>))
import Extra.Basics
import Extra.List
import Extra.Ops

data MList m a where
  MNull :: MList m a
  MCons :: a -> m (MList m a) -> MList m a

newtype ChoiceT m a = ChoiceT {runChoiceT :: m (MList m a)}
mlistAppend :: (Functor m) => MList m a -> MList m a -> MList m a
mlistAppend MNull ys = ys
mlistAppend (MCons x xs) ys = MCons x (xs <&> \xs' -> mlistAppend xs' ys)

instance (Monad m) => Semigroup (MList m a) where
  (<>) = mlistAppend

instance (Monad m) => Monoid (MList m a) where
  mempty = MNull

instance (Monad m) => Functor (MList m) where
  fmap f MNull = MNull
  fmap f (MCons x xs) = MCons (f x) (fmap (fmap f) xs)

instance (Monad m) => Applicative (MList m) where
  pure x = MCons x (pure MNull)
  MNull <*> _ = MNull
  _ <*> MNull = MNull
  -- MCons f fs <*> MCons x xs = MCons (f x) ((f <$$> xs) <> (fs <*> pure x) <> (fs <*> xs))
  MCons f fs <*> MCons x xs = MCons (f x) $ do
    fs' <- fs
    xs' <- xs
    pure $ fs' <*> xs'

bindMList :: forall m a b. (Monad m) => (a -> MList m b) -> MList m a -> MList m b
bindMList f MNull = MNull
bindMList f x = _

-- TODO:
instance (Monad m) => Monad (MList m) where
  return = pure
instance (Monad m) => Functor (ChoiceT m) where
  fmap f (ChoiceT xs) = ChoiceT (fmap (fmap f) xs)

instance (Monad m) => Semigroup (ChoiceT m a) where
  (ChoiceT l0) <> (ChoiceT l1) = ChoiceT $ mlistAppend <$> l0 <*> l1
instance (Monad m) => Monoid (ChoiceT m a) where
  mempty = ChoiceT (pure MNull)

instance (Monad m) => Applicative (ChoiceT m) where
  pure x = ChoiceT (pure (MCons x (pure MNull)))
  ChoiceT f' <*> ChoiceT x' = ChoiceT $ do
    ft <- f'
    xt <- x'
    case (ft, xt) of
      (MNull, _) -> pure MNull
      (_, MNull) -> pure MNull
      (MCons f fs, MCons x xs) -> do
        let r0 = f x
        fs' <- fs
        xs' <- xs
        let r1 = f <$> xs'
        let r2 = fs' <*> xs'
        let r3 = fs' <*> xs'
        pure $ (pure r0) <> r1 <> r2 <> r3

bindChoice :: (Monad m) => (a -> ChoiceT m b) -> ChoiceT m a -> ChoiceT m b
bindChoice f (ChoiceT xs) = _
