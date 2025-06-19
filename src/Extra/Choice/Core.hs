{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.Choice.Core where

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
import Extra ((<&>))
import Extra.Basics
import Extra.Choice.Types
import Extra.List
import Extra.Ops

-- Implemenation of join
joinListT :: (Functor m, Monad m) => ChoiceT m (ChoiceT m a) -> ChoiceT m a
joinListT (ChoiceT xss) = ChoiceT . joinMList $ fmap (fmap _runChoiceT) xss

joinMList :: (Functor m, Monad m) => MList m (MList m a) -> MList m a
joinMList = (=<<) joinMList'

joinMList' :: (Functor m, Monad m) => MStep m (MList m a) -> MList m a
joinMList' MNil = return MNil
joinMList' (MCons x xs) = x `mAppend` joinMList xs

mAppend :: (Functor m, Monad m) => MList m a -> MList m a -> MList m a
mAppend xs ys = (`mAppend'` ys) =<< xs

mAppend' :: (Functor m, Monad m) => MStep m a -> MList m a -> MList m a
mAppend' MNil ys = ys
mAppend' (MCons x xs) ys = return $ MCons x (mAppend xs ys)

-- A "lazy" run function, which only calculates the first solution.
runListT' :: (Functor m) => ChoiceT m a -> m (Maybe (a, ChoiceT m a))
runListT' (ChoiceT m) = fmap g m
 where
  g MNil = Nothing
  g (MCons x xs) = Just (x, ChoiceT xs)

-- In ChoiceT from Control.Monad this one is the data constructor ChoiceT, so sadly, this code can't be a drop-in replacement.
liftList :: (Monad m) => [a] -> ChoiceT m a
liftList [] = ChoiceT $ return MNil
liftList (x : xs) = ChoiceT . return $ MCons x (_runChoiceT $ liftList xs)

instance (Functor m) => Functor (MStep m) where
  fmap _ MNil = MNil
  fmap f (MCons x xs) = MCons (f x) (fmap (fmap f) xs)

bindStep :: (Monad m) => MStep m (a -> b) -> MStep m a -> MStep m b
bindStep (MCons f fs) (MCons x xs) = MCons (f x) $ do
  (fs' :: MStep m (a -> b)) <- fs
  (xs' :: MStep m (a)) <- xs
  let r1 = f <$> xs'
  let r2 = fs' <*> pure x
  let r3 = fs' <*> xs'
  let rt = mAppend' r1 $ mAppend' r2 $ pure r3
  rt
{-# INLINE bindStep #-}
instance (Monad m) => Applicative (MStep m) where
  pure x = MCons x (pure MNil)
  (<*>) :: forall a b. MStep m (a -> b) -> MStep m a -> MStep m b
  MNil <*> _ = MNil
  _ <*> MNil = MNil
  x <*> y = bindStep x y
