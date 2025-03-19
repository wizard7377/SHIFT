module Extra.Choice where

import Control.Applicative
import Data.Maybe (catMaybes)
import Extra.Basics
import Extra.List

{- | The model of choice
For a given type `a`, the inner list represents a choice of `a`
That is, it reperesents a non-deterministic choice of `a`, all possibilities
-}
newtype Choice a = Choice [a]
  deriving (Show, Eq)

-- | Change every instnace of the inner value
instance Functor Choice where
  fmap :: (a -> b) -> Choice a -> Choice b
  fmap f (Choice choice) = Choice $ f <$> choice

-- | Take every possible function, and every possible input, and cross apply them, then take the sum (or) of their results
instance Applicative Choice where
  pure :: a -> Choice a
  pure a = Choice [a]
  (<*>) :: Choice (a -> b) -> Choice a -> Choice b
  (Choice func) <*> (Choice val) = Choice $ func <*> val

-- | If the inner values are a semigroup, then cross apply their concatenation
instance (Semigroup a) => Semigroup (Choice a) where
  (<>) = choiceAnd

instance (Monoid a) => Monoid (Choice a) where
  mempty = Choice [mempty]
  mappend = (<>)

-- | Simply add the choices together
instance Alternative Choice where
  empty = Choice []
  (<|>) = choiceOr

{- | Take every single possible input, and feed it into the function, which will give @Choice (Choice a)@
 Then, simply flatten by `<|>`
-}
instance Monad Choice where
  (>>=) :: Choice a -> (a -> Choice b) -> Choice b
  (Choice v0) >>= f =
    let
      v1 = f <$> v0
      v2 = foldr (<|>) empty v1
     in
      v2

instance Foldable Choice where
  foldMap :: (Monoid m) => (a -> m) -> Choice a -> m
  foldMap f (Choice choice) = foldMap f choice
instance Traversable Choice where
  traverse :: (Applicative f) => (a -> f b) -> Choice a -> f (Choice b)
  traverse f (Choice choice) = Choice <$> traverse f choice

-- | The simple choice, that is, (A | F)
simple a = pure a

trivial = Choice [[]]
cabsurd = Choice []
choiceAnd :: (Semigroup a) => Choice a -> Choice a -> Choice a
choiceAnd (Choice as) (Choice bs) = Choice $ (<>) <$> as <*> bs
choiceOr :: Choice a -> Choice a -> Choice a
choiceOr (Choice as) (Choice bs) = Choice $ as ++ bs

capply :: (a -> b) -> Choice a -> Choice b
capply f (Choice as) = Choice $ f <$> as
cfilterMap :: (a -> Maybe b) -> Choice a -> Choice b
cfilterMap f (Choice as) = Choice $ catMaybes $ f <$> as
cfilter f (Choice as) = Choice $ filter f as
(<&&>) :: (Semigroup a) => Choice a -> Choice a -> Choice a
(<||>) :: (Semigroup a) => Choice a -> Choice a -> Choice a
(<&&>) = choiceAnd
(<||>) = choiceOr
infixl 9 <&&>
infixl 8 <||>

solve :: (a -> Bool) -> Choice a -> Choice a
solve prop (Choice choice) = Choice $ filter prop choice
resolve :: Choice a -> [a]
resolve (Choice choice) = choice
