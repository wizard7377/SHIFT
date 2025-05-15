{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.Monad where

import Control.Applicative (liftA)
import Control.Monad
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (..), runExceptT)
import Control.Monad.ListT.Funcs (fromList)
import Control.Monad.RWS
import Control.Monad.RWS qualified as RWS
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
import Extra.Error (Error)
import Rift (Term')
import Rift qualified

{- | The central monad transformer
  Takes in five parameters, in order:
  - [@t@] The first is the theory type, `t`, which is the enviroment, which is itself wrapped in `Rift.LogicEnv`, with `MonadReader`
  - [@w@] Writer `w`, over `MonadWriter`
  - [@s@] The third is the state `s`, over `MonadState`
  - [@m@] Inner monad `m`
  - [@a@] Action `a`
  In addition, it adds the ability of `MonadError` with `Error`
-}
newtype LMT (t :: Type) (w :: Type) (s :: Type) (m :: Type -> Type) (a :: Type) = LMT
  { -- unLMT :: ChoiceT (ExceptT Error (RWST LogicEnv w s m)) a
    unLMT :: (Rift.LogicEnv t) -> s -> m (Either Error a, s, w)
  -- ^ The functional internals
  }

bindLMT :: (Monad m, Semigroup w) => LMT r w s m a -> (a -> LMT r w s m b) -> LMT r w s m b
bindLMT comp f = LMT $ \env s -> do
  (res, s', w) <- runLMT comp env s
  case res of
    Left err -> pure (Left err, s', w)
    Right x -> do
      (res', s'', w') <- runLMT (f x) env s'
      pure (res', s'', w <> w')

-- unLMT :: (ExceptT Error (RWS.RWST (Rift.LogicEnv r) w s m)) a

instance (Monad m, Semigroup w) => Functor (LMT r w s m) where
  fmap :: (a -> b) -> LMT r w s m a -> LMT r w s m b
  fmap f (LMT xs) = LMT $ \env s -> do
    (res, s', w) <- xs env s
    pure (fmap f res, s', w)

instance (Monad m, Monoid w) => Applicative (LMT r w s m) where
  pure :: a -> LMT r w s m a
  pure a = LMT $ \_ s -> pure (Right a, s, mempty)
  (<*>) ::
    (Monad m, Monoid w) =>
    LMT r w s m (a -> b) ->
    LMT r w s m a ->
    LMT r w s m b
  LMT xs <*> LMT ys = LMT $ \env s -> do
    (res, s', w) <- xs env s
    case res of
      Left err -> pure (Left err, s', w)
      Right f -> do
        (res', s'', w') <- ys env s'
        pure (fmap f res', s'', w <> w')

instance (Monad m, Monoid w) => Monad (LMT r w s m) where
  (>>=) ::
    LMT r w s m a ->
    (a -> LMT r w s m b) ->
    LMT r w s m b
  (>>=) = bindLMT

instance (Monoid w, Monad m) => MonadState s (LMT r w s m) where
  get :: LMT r w s m s
  get = LMT $ \_ s -> pure (Right s, s, mempty)
  put :: s -> LMT r w s m ()
  put s = LMT $ \_ _ -> pure (Right (), s, mempty)

instance (Monad m, Monoid w) => MonadReader (Rift.LogicEnv r) (LMT r w s m) where
  ask :: LMT r w s m (Rift.LogicEnv r)
  ask = LMT $ \env s -> pure (Right env, s, mempty)
  local ::
    (Rift.LogicEnv r -> Rift.LogicEnv r) ->
    LMT r w s m a ->
    LMT r w s m a
  local f (LMT xs) = LMT $ \env s -> do
    (res, s', w) <- xs (f env) s
    pure (res, s', w)

instance (Monad m, Monoid w) => MonadError Error (LMT r w s m) where
  throwError :: Error -> LMT r w s m a
  throwError err = LMT $ \_ s -> pure (Left err, s, mempty)
  catchError ::
    LMT r w s m a ->
    (Error -> LMT r w s m a) ->
    LMT r w s m a
  catchError (LMT xs) f = LMT $ \env s -> do
    (res, s', w) <- xs env s
    case res of
      Left err -> runLMT (f err) env s'
      Right x -> pure (Right x, s', w)

instance (Monoid w) => MonadTrans (LMT r w s) where
  lift :: (Monad m) => m a -> LMT r w s m a
  lift x = LMT $ \_ s -> do
    res <- x
    pure (Right res, s, mempty)

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM r w s = LMT r w s Identity

-- | Takes in a `me` state and a `Rift.Theory` @t@, for which we have a mapping @t -> me@
class EnterState me where
  type TermOf me

  enterState :: (Rift.Theory t) => (Rift.TermOf t ~ TermOf me) => Rift.LogicEnv t -> me

runLMT = unLMT
mkLMT = LMT
runLMT' :: LMT r w s m a -> Rift.LogicEnv r -> s -> m (Either Error a, s, w)
runLMT' (LMT xs) env s = xs env s
mkLMT' :: (Rift.LogicEnv r -> s -> m (Either Error a, s, w)) -> LMT r w s m a
mkLMT' = LMT
applyEnv :: forall t r w s m a. (Monad m, Monoid w, EnterState s, TermOf s ~ Rift.TermOf r, Rift.Theory r) => LMT r w s m a -> Rift.LogicEnv r -> m (Either Error a, s, w)
applyEnv comp env = runLMT' comp env (enterState env)
