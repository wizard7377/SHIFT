{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

-- {-# LANGUAGE UndecidableInstances #-}

{- |
Module      : Data.Choice.Core
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Data.Choice.Core where

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
import Data.Choice.Types

-- | Monadic join for 'ChoiceT'. Flattens a 'ChoiceT' of 'ChoiceT's into a single 'ChoiceT'.
joinListT :: (Functor m, Monad m) => ChoiceT m (ChoiceT m a) -> ChoiceT m a
joinListT (ChoiceT xss) = ChoiceT . joinMList $ fmap (fmap _runChoiceT) xss
{-# INLINE joinListT #-}

-- | Monadic join for 'ListT'. Flattens a 'ListT' of 'ListT's into a single 'ListT'.
joinMList :: (Functor m, Monad m) => ListT m (ListT m a) -> ListT m a
joinMList = (=<<) joinMList'
{-# INLINE joinMList #-}

-- | Helper for 'joinMList'. Processes a single step of a 'ListT' of 'ListT's.
joinMList' :: (Functor m, Monad m) => StepT m (ListT m a) -> ListT m a
joinMList' MNil = return MNil
joinMList' (MCons x xs) = x `mAppend` joinMList xs

-- | Monadic append for 'ListT'. Concatenates two 'ListT' computations.
mAppend :: (Functor m, Monad m) => ListT m a -> ListT m a -> ListT m a
mAppend xs ys = (`mAppend'` ys) =<< xs

mAppendListT :: (Functor m, Monad m) => ListT m a -> ListT m a -> ListT m a
mAppendListT = mAppend

-- | Helper for 'mAppend'. Appends a single step to a 'ListT'.
mAppend' :: (Functor m, Monad m) => StepT m a -> ListT m a -> ListT m a
mAppend' MNil ys = ys
mAppend' (MCons x xs) ys = return $ MCons x (mAppend xs ys)

-- | Runs a 'ListT' computation and collects the results in a list.
execListT :: (Monad m) => ListT m a -> m [a]
execListT m = do
  xs <- m
  case xs of
    MNil -> return []
    MCons x xs' -> (x :) <$> execListT (xs')

-- | Lazily runs a 'ChoiceT' computation, returning the first solution and the remaining computation, if any.
runListT' :: (Functor m) => ChoiceT m a -> m (Maybe (a, ChoiceT m a))
runListT' (ChoiceT m) = fmap g m
 where
  g MNil = Nothing
  g (MCons x xs) = Just (x, ChoiceT xs)

-- | Lifts a list into the 'ChoiceT' monad transformer.
liftList :: (Monad m) => [a] -> ChoiceT m a
liftList [] = ChoiceT $ return MNil
liftList (x : xs) = ChoiceT . return $ MCons x (_runChoiceT $ liftList xs)

-- | Converts a foldable structure to a 'ListT'.
fromListList :: (Monad m, Foldable f) => f a -> ListT m a
fromListList (toList -> []) = return MNil
fromListList (toList -> (x : xs)) = return $ MCons x (fromListList xs)

-- | Converts a foldable structure to a 'ChoiceT'.
fromList :: (Monad m, Foldable f) => f a -> ChoiceT m a
fromList xs = ChoiceT $ fromListList xs

-- | Converts a 'ChoiceT' computation to a list in the base monad.
ctoList :: (Monad m) => ChoiceT m a -> m [a]
ctoList (ChoiceT m) = do
  xs <- m
  case xs of
    MNil -> return []
    MCons x xs' -> (x :) <$> ctoList (ChoiceT xs')

instance (Functor m) => Functor (StepT m) where
  fmap _ MNil = MNil
  fmap f (MCons x xs) = MCons (f x) (fmap (fmap f) xs)

-- | Applicative-style bind for 'MStep'.
bindStep :: (Monad m) => StepT m (a -> b) -> StepT m a -> StepT m b
bindStep (MCons f fs) (MCons x xs) = MCons (f x) $ do
  (fs' :: StepT m (a -> b)) <- fs
  (xs' :: StepT m (a)) <- xs
  let r1 = f <$> xs'
  let r2 = fs' <*> pure x
  let r3 = fs' <*> xs'
  let rt = mAppend' r1 $ mAppend' r2 $ pure r3
  rt
{-# INLINE bindStep #-}

instance (Monad m) => Applicative (StepT m) where
  pure x = MCons x (pure MNil)
  (<*>) :: forall a b. StepT m (a -> b) -> StepT m a -> StepT m b
  MNil <*> _ = MNil
  _ <*> MNil = MNil
  x <*> y = bindStep x y
