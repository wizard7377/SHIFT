{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

module Extra.Choice.Instances where

-- import Control.Applicative
import Control.Applicative (Alternative (..), Applicative (..))

-- import Control.Arrow
import Control.Category
import Control.Comonad
import Control.Monad
import Control.Monad.Accum
import Control.Monad.Cont
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.Morph
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor ((<&>))
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
import Prelude hiding (id, (.))

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

instance {-# OVERLAPS #-} (Monad m) => Alternative (ChoiceT m) where
  empty = ChoiceT $ pure MNil
  (<|>) :: (Monad m) => ChoiceT m a -> ChoiceT m a -> ChoiceT m a
  f <|> g = do
    f' <- cget f
    g' <- cget g
    case (f', g') of
      (Nothing, Nothing) -> empty
      (Just (a, al), Nothing) -> cset $ pure $ Just (a, al)
      (Nothing, Just (b, bl)) -> cset $ pure $ Just (b, bl)
      (Just (a, al), Just (b, bl)) -> cset (pure $ Just (a, pure b)) <|> (al <|> bl)

instance (Monad m) => MonadChoice (ChoiceT m) where
  cset :: (Monad m) => Yield (ChoiceT m) a -> ChoiceT m a
  cset x' = do
    x <- x'
    case x of
      Nothing -> empty
      Just (a, (ChoiceT xs)) -> do
        ChoiceT $ pure $ MCons a xs
  cget :: (Monad m) => ChoiceT m a -> Yield (ChoiceT m) a
  cget (ChoiceT x') = ChoiceT $ do
    x <- x'
    case x of
      MNil -> pure $ pure Nothing
      MCons a xs -> pure $ pure $ Just (a, ChoiceT xs)

instance Functor (ChoiceAT a m)

instance (MonadReader e m) => MonadReader e (ChoiceT m) where
  ask = lift ask
  local f (ChoiceT m) = ChoiceT $ local f m

instance (MonadState s m) => MonadState s (ChoiceT m) where
  get = lift get
  put = lift . put

instance (MonadAccum s m) => MonadAccum s (ChoiceT m) where
  look :: (MonadAccum s m) => ChoiceT m s
  look = lift look
  add :: (MonadAccum s m) => s -> ChoiceT m ()
  add = lift . add

instance {-# OVERLAPPABLE #-} (Applicative m, MonadChoice m) => Alternative m where
  empty :: (Applicative m, MonadChoice m) => m a
  empty = cset $ pure Nothing
  (<|>) :: (Applicative m, MonadChoice m) => m a -> m a -> m a
  f <|> g = do
    f' <- cget f
    g' <- cget g
    case (f', g') of
      (Nothing, Nothing) -> empty
      (Just (a, al), Nothing) -> f
      (Nothing, Just (b, bl)) -> g
      (Just (a, al), Just (b, bl)) -> cset (pure $ Just (a, pure b)) <|> (al <|> bl)

instance MFunctor ChoiceT where
  hoist :: (Monad m) => (forall a. m a -> n a) -> ChoiceT m b -> ChoiceT n b
  hoist f (ChoiceT x) = ChoiceT $ go x
   where
    go run =
      f $
        run <&> \case
          MCons elem next -> MCons elem (go next)
          MNil -> MNil

squashChoice :: forall a m. (Monad m) => ChoiceT (ChoiceT m) a -> ChoiceT m a
squashChoice (ChoiceT m) = go m
 where
  (go :: ListT (ChoiceT m) a -> ChoiceT m a) = \f' -> do
    f <- f'
    case f of
      MNil -> empty
      MCons x xs -> (pure x) <|> (go xs)

embedList ::
  forall m n b.
  (Monad n) =>
  (forall a. m a -> ListT n a) ->
  ListT m b ->
  ListT n b
embedList f m =
  go (f m)
 where
  go :: ListT n (StepT m b) -> ListT n b
  go x = do
    x' <- x
    case x' of
      MNil -> pure MNil
      MCons MNil xs -> go xs
      MCons (MCons h t) xs -> (mAppendListT (pure $ MCons h $ pure MNil) (mAppendListT (embedList f t) (go xs)))
instance MMonad ChoiceT where
  embed ::
    (Monad n) =>
    (forall a. m a -> ChoiceT n a) ->
    ChoiceT m b ->
    ChoiceT n b
  embed f (ChoiceT m) = ChoiceT $ embedList (_runChoiceT . f) m
