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
import Data.List.Extra (nubBy)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra.Basics
import Extra.List
import Extra.Ops

type Choice :: Type -> Type
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

{-# INLINE bindChoice #-}
bindChoice :: Choice a -> (a -> Choice b) -> Choice b
bindChoice (Choice xs) f = Choice $ concatMap (runChoice . f) xs
instance Monad Choice where
  (>>=) = bindChoice

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

traverseChoice :: (Applicative f) => (a -> f b) -> Choice a -> f (Choice b)
traverseChoice f (Choice xs) = Choice <$> traverse f xs
instance Traversable Choice where
  traverse f (Choice xs) = Choice <$> traverse f xs

runChoice :: Choice a -> [a]
runChoice (Choice xs) = xs
mkChoice :: [a] -> Choice a
mkChoice = Choice

cabsurd :: Choice a
cabsurd = Choice []

cexists :: Choice a -> Bool
cexists (Choice xs) = not $ null xs
csimpl :: (a -> a -> Bool) -> Choice a -> Choice a
csimpl f (Choice xs) = Choice $ nubBy f xs
csimpl' :: (Eq a) => Choice a -> Choice a
csimpl' = csimpl (==)
csubset :: (a -> b -> Bool) -> Choice a -> Choice b -> Bool
csubset f (Choice xs) (Choice ys) = all (\x -> any (f x) ys) xs
csubset' :: (Eq a) => Choice a -> Choice a -> Bool
csubset' = csubset (==)
