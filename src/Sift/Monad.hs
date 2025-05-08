{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.Monad where

import Control.Applicative (liftA)
import Control.Monad
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (..), runExceptT)
import Control.Monad.ListT.Funcs (fromList)
import Control.Monad.RWS
import Control.Monad.Reader (MonadReader (..), ReaderT)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans as Trans
import Control.Monad.Writer (MonadWriter (..))
import Data.Bifunctor (second)
import Data.Foldable (asum)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.List (singleton)
import Data.Typeable
import Extra
import Extra.Choice
import Extra.Error (Error)
import Rift (Term)
import Rift qualified
import Sift.Base (LogicEnv, defaultEnv)

{- |
 The central and most general logic monad transformer.
 Takes in a `LogicEnv`, a state `s`, and a monad `m`, and is equipped with a writter `w` and a potential throw
 Approx `r -> s -> w -> m (w, ChoiceT (Either Error a))`
-}
newtype LMT (s :: Type) (w :: Type) (m :: Type -> Type) (a :: Type) = LMT
  { -- unLMT :: ChoiceT (ExceptT Error (RWST LogicEnv w s m)) a
    unLMT :: (ExceptT Error (RWST LogicEnv w s m)) (Choice a)
  -- ^ The functional internals
  }

instance (Monad m) => Functor (LMT s w m) where
  fmap :: (Monad m) => (a -> b) -> LMT s w m a -> LMT s w m b
  fmap f (LMT xs) = LMT $ fmap f <$> xs

instance (Monad m, Monoid w) => Applicative (LMT s w m) where
  pure :: (Monad m, Monoid w) => a -> LMT s w m a
  pure = LMT . pure . singleton
  (<*>) ::
    (Monad m, Monoid w) =>
    LMT s w m (a -> b) ->
    LMT s w m a ->
    LMT s w m b
  LMT fs <*> LMT xs = LMT $ liftA2 (<*>) fs xs

instance (Monad m, Monoid w, Traversable m) => Monad (LMT s w m) where
  (>>=) ::
    (Monad m, Monoid w, Traversable m) =>
    LMT s w m a ->
    (a -> LMT s w m b) ->
    LMT s w m b
  LMT xs >>= f = LMT $ xs >>= \x -> unLMT (flattenChoice $ mapM f x)

instance (Monoid w, Traversable m, Monad m) => MonadState s (LMT s w m) where
  get :: (Monoid w, Traversable m, Monad m) => LMT s w m s
  get = LMT $ ExceptT $ RWST $ \env s -> pure (Right [s], s, mempty)
  put :: (Monoid w, Traversable m, Monad m) => s -> LMT s w m ()
  put s = LMT $ ExceptT $ RWST $ \env _ -> pure (Right [()], s, mempty)

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM s = LMT s () Identity

-- | Takes in some sentence type, and a `me` type, and returns calss of values which can be made from the sentences
class EnterState (me :: Type -> Type) where
  -- enterState :: (Rift.Sentence sen Term) => LogicEnv -> [sen atom] -> me sen atom
  enterState :: LogicEnv -> [Term atom] -> [Term atom] -> me atom

runLMT = unLMT
mkLMT = LMT
runLMT' :: LMT s w m a -> LogicEnv -> s -> m (Either Error [a], s, w)
runLMT' comp = runRWST (runExceptT $ (runLMT comp))
mkLMT' :: (LogicEnv -> s -> m (Either Error [a], s, w)) -> LMT s w m a
mkLMT' comp = LMT $ ExceptT $ RWST comp
fromChoice :: (Monad m, Monoid w) => Choice a -> LMT s w m a
fromChoice xs = LMT (pure xs)
flattenChoice :: (Monad m, Monoid w) => LMT s w m [a] -> LMT s w m a
flattenChoice (LMT xs) = LMT $ fmap concat xs
