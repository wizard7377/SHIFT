{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.Choice where

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
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra.Basics
import Extra.List
import Extra.Ops

newtype Choice a = Choice [a]
  deriving (Data, Typeable, Generic)
instance (Show a) => Show (Choice a) where
  show (Choice xs) = "?" ++ show xs
instance (Eq a) => Eq (Choice a) where
  (Choice xs) == (Choice ys) = xs == ys
instance (Ord a) => Ord (Choice a) where
  compare (Choice xs) (Choice ys) = compare xs ys
instance Functor Choice where
  fmap f (Choice xs) = Choice $ map f xs
instance Applicative Choice where
  pure x = Choice [x]
  (Choice fs) <*> (Choice xs) = Choice $ [f x | f <- fs, x <- xs]
instance Monad Choice where
  (Choice xs) >>= f =
    Choice $
      concatMap
        ( \x -> case f x of
            Choice ys -> ys
        )
        xs

instance (Semigroup a) => Semigroup (Choice a) where
  (Choice xs) <> (Choice ys) = Choice $ (<>) <$> xs <*> ys

instance (Monoid a) => Monoid (Choice a) where
  mempty = Choice [mempty]

instance Alternative Choice where
  empty = Choice []
  (Choice xs) <|> (Choice ys) = Choice $ xs <|> ys

instance MonadPlus Choice where
  mzero = empty
  mplus = (<|>)

instance Foldable Choice where
  foldr f z (Choice xs) = foldr f z xs

instance Traversable Choice where
  traverse f (Choice xs) = Choice <$> traverse f xs
  sequenceA (Choice xs) = Choice <$> sequenceA xs

runChoice :: Choice a -> [a]
runChoice (Choice xs) = xs
mkChoice :: [a] -> Choice a
mkChoice = Choice
