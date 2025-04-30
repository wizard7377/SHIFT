{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.ChoiceOld where

import Control.Applicative
import Data.Functor.Identity (Identity (..))
import Data.Maybe (catMaybes)
import Extra.Basics
import Extra.List
import Extra.Ops

{- | The model of choice
For a given type `a`, the inner list represents a choice of `a`
That is, it reperesents a non-deterministic choice of `a`, all possibilities
-}
newtype ChoiceT m a = Choice (m [a])

type Choice = ChoiceT Identity

deriving instance (Show (m [a])) => Show (ChoiceT m a)

deriving instance (Eq (m [a])) => Eq (ChoiceT m a)

-- | Change every instnace of the inner value
instance Functor Choice where
  fmap :: (a -> b) -> Choice a -> Choice b
  fmap f (Choice choice) = Choice $ f <$$> choice

-- | Take every possible function, and every possible input, and cross apply them, then take the sum (or) of their results
instance Applicative Choice where
  pure :: a -> Choice a
  pure a = Choice (pure [a])
  (<*>) :: Choice (a -> b) -> Choice a -> Choice b
  (Choice func) <*> (Choice val) = Choice $ Identity $ ((runIdentity func) <*> (runIdentity val))

instance (Semigroup a) => Semigroup (Choice a) where
  (<>) = choiceAnd

instance (Monoid a) => Monoid (Choice a) where
  mempty = Choice (pure [mempty])
  mappend = (<>)

-- | Simply add the choices together
instance Alternative Choice where
  empty = Choice mempty
  (<|>) = choiceOr

{- | Take every single possible input, and feed it into the function, which will give @Choice (Choice a)@
Then, simply flatten by `<|>`
-}
instance Monad Choice where
  (>>=) :: Choice a -> (a -> Choice b) -> Choice b
  (Choice v0) >>= f =
    let v1 = f <$$> v0
        v2 = foldr (<|>) empty (runIdentity v1)
     in v2

instance Foldable Choice where
  foldMap :: (Monoid m) => (a -> m) -> Choice a -> m
  foldMap f (Choice choice) = foldMap f (runIdentity choice)

-- instance Traversable Choice where

-- | The simple choice, that is, (A | F)
simple a = pure a

trivial = Choice $ Identity [[]]

cabsurd = Choice $ Identity []

choiceAnd :: (Semigroup a) => Choice a -> Choice a -> Choice a
choiceAnd (Choice as) (Choice bs) = Choice $ (<>) <$> as <*> bs

choiceOr :: Choice a -> Choice a -> Choice a
choiceOr (Choice as) (Choice bs) = Choice $ (++) <$> as <*> bs

choice :: [a] -> Choice a
choice input = Choice $ Identity input

capply :: (a -> b) -> Choice a -> Choice b
capply f (Choice as) = Choice $ f <$$> as

cfilterMap :: (a -> Maybe b) -> Choice a -> Choice b
cfilterMap f (Choice as) = Choice $ catMaybes <$> (f <$>) <$> as

cfilter f (Choice as) = choice $ filter f (runIdentity as)

(<&&>) :: (Semigroup a) => Choice a -> Choice a -> Choice a
(<||>) :: (Semigroup a) => Choice a -> Choice a -> Choice a
(<&&>) = choiceAnd

(<||>) = choiceOr

infixl 9 <&&>

infixl 8 <||>

solve :: (a -> Bool) -> Choice a -> Choice a
solve prop (Choice choice) = Choice $ filter prop <$> choice

resolve :: Choice a -> [a]
resolve (Choice choice) = runIdentity $ choice
