{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Choice where

import Control.Applicative
import Control.Category qualified as Category
import Control.Comonad
import Control.Monad
import Control.Monad.Accum
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

data ChoiceAccum m a = Choice m [a]
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
type Choice = ChoiceAccum ()
type (>+>) = ChoiceArrow
newtype ChoiceArrow w a b = ChoiceArrow (ChoiceAccum w a -> ChoiceAccum w b)
instance (Monoid w) => Category.Category (ChoiceArrow w) where
  id = ChoiceArrow $ \(Choice w x) -> Choice w x
  (.) ::
    (Monoid w) =>
    ChoiceArrow w b c ->
    ChoiceArrow w a b ->
    ChoiceArrow w a c
  ChoiceArrow g . ChoiceArrow f = ChoiceArrow $ \(Choice w x) ->
    let
      Choice w0 r0 = f (Choice w x)
      Choice w1 r1 = g (Choice w0 r0)
     in
      Choice w1 r1
instance Functor (ChoiceAccum m) where
  fmap f (Choice m xs) = Choice (m) (fmap f xs)
instance (Monoid m) => Applicative (ChoiceAccum m) where
  pure x = Choice (mempty) ([x])
  Choice m1 fs <*> Choice m2 xs = Choice (m1 <> m2) (concatMap (\f -> fmap f xs) fs)

bindChoice :: (Monoid m) => ChoiceAccum m a -> (a -> ChoiceAccum m b) -> ChoiceAccum m b
bindChoice (Choice (m) (input)) f =
  case input of
    (x : xs) ->
      let
        Choice m0 r0 = f x
        Choice m1 rest = bindChoice (Choice m0 xs) f
       in
        Choice m1 (r0 <> rest)
    [] -> Choice (m) ([])
instance (Monoid m) => Monad (ChoiceAccum m) where
  return = pure
  (>>=) = bindChoice
instance (Semigroup m) => Semigroup (ChoiceAccum m a) where
  Choice m1 xs1 <> Choice m2 xs2 = Choice (m1 <> m2) (xs1 <> xs2)

instance (Monoid m) => Monoid (ChoiceAccum m a) where
  mempty = Choice (mempty) ([])
  mappend = (<>)

instance (Monoid m) => Alternative (ChoiceAccum m) where
  empty = Choice (mempty) ([])
  Choice m1 xs1 <|> Choice m2 xs2 = Choice (m1 <> m2) (xs1 <> xs2)
instance (Monoid m) => MonadPlus (ChoiceAccum m) where
  mzero = Choice (mempty) ([])
  mplus (Choice m1 xs1) (Choice m2 xs2) = Choice (m1 <> m2) (xs1 <> xs2)

instance (Monoid acc) => MonadState acc (ChoiceAccum acc) where
  state :: (Monoid acc) => (acc -> (a, acc)) -> ChoiceAccum acc a
  state f =
    let
      (x, acc) = f mempty
     in
      Choice acc [x]
instance Foldable (ChoiceAccum m) where
  foldMap f (Choice m xs) = foldMap f xs
  foldr f z (Choice m xs) = foldr f z xs
  foldl f z (Choice m xs) = foldl f z xs
  toList (Choice _ xs) = xs
instance Traversable (ChoiceAccum m) where
  traverse f (Choice m xs) = Choice m <$> traverse f xs
  sequenceA (Choice m xs) = Choice m <$> sequenceA xs
instance Bifunctor ChoiceAccum where
  bimap f g (Choice m xs) = Choice (f m) (fmap g xs)

cabsurd :: (Monoid m) => ChoiceAccum m a
cabsurd = Choice (mempty) ([])
csimpl :: (a -> a -> Bool) -> ChoiceAccum m a -> ChoiceAccum m a
csimpl eq (Choice m xs) =
  let
    xs' = nubBy eq xs
   in
    Choice m xs'

csimpl' :: (Eq a) => ChoiceAccum m a -> ChoiceAccum m a
csimpl' = csimpl (==)
cexists :: ChoiceAccum m a -> Bool
cexists (Choice _ xs) = not $ null xs
mkChoice :: (Monoid m) => [a] -> ChoiceAccum m a
mkChoice x = Choice (mempty) x
mkChoice1 :: (Monoid m) => a -> ChoiceAccum m a
mkChoice1 x = Choice (mempty) (singleton x)
runChoice :: (Monoid m) => ChoiceAccum m a -> [a]
runChoice (Choice _ xs) = xs
runChoiceAccum :: (Monoid m) => ChoiceAccum m a -> (m, [a])
runChoiceAccum (Choice m xs) = (m, xs)
