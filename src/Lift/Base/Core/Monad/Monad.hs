module Lift.Base.Core.Monad.Monad where

import Control.Monad.Error.Class
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Writer
import Data.Text qualified as T
import Extra
import Extra.Error
import Lift.Base.Core.Forms
import Lift.Base.Core.Forms.Message
import Lift.Base.Core.Forms.Module
import Lift.Base.Core.ParseEnv

newtype FMT t m a = FMT {runFMT :: ParseEnv -> LiftState t -> m (Either Error a, LiftState t, [Message])}

instance (Monad m) => Functor (FMT t m) where
  fmap f (FMT g) = FMT $ \env universe -> do
    (a, universe', messages) <- g env universe
    pure (f <$> a, universe', messages)
instance (Monad m) => Applicative (FMT t m) where
  pure a = FMT $ \_ universe -> pure (Right a, universe, [])
  FMT f <*> FMT g = FMT $ \env universe -> do
    (f', universe', messages) <- f env universe
    (a, universe'', messages') <- g env universe'
    pure (f' <*> a, universe'', messages <> messages')
instance (Monad m) => Monad (FMT t m) where
  FMT f >>= g = FMT $ \env universe -> do
    (a, universe', messages) <- f env universe
    case a of
      (Right a') -> do
        let FMT h = g a'
        (b, universe'', messages') <- h env universe'
        pure (b, universe'', messages <> messages')
      (Left e) -> pure (Left e, universe', messages)
instance MonadTrans (FMT t) where
  lift m = FMT $ \_ universe -> do
    a <- m
    pure (Right a, universe, [])
instance (Monad m) => MonadReader ParseEnv (FMT t m) where
  ask = FMT $ \env universe -> pure (Right env, universe, [])
  local f (FMT g) = FMT $ \env universe -> g (f env) universe
instance (Monad m) => MonadWriter [Message] (FMT t m) where
  tell messages = FMT $ \_ universe -> pure (Right (), universe, messages)
  listen (FMT g) = FMT $ \env universe -> do
    (a, universe', messages) <- g env universe
    pure ((,messages) <$> a, universe', messages)
  pass (FMT g) = FMT $ \env universe -> do
    (out, universe', messages) <- g env universe
    case out of
      (Right (a, f)) -> do
        let messages' = f messages
        pure (Right a, universe', messages')
      (Left e) -> pure (Left e, universe', messages)
instance (Monad m) => MonadState (LiftState t) (FMT t m) where
  get = FMT $ \_ universe -> pure (Right universe, universe, [])
  put universe = FMT $ \_ _ -> pure (Right (), universe, [])

instance (Monad m) => MonadError Error (FMT t m) where
  throwError err = FMT $ \_ universe -> pure (Left err, universe, [])
  catchError (FMT f) h = FMT $ \env universe -> do
    (a, universe', messages) <- f env universe
    case a of
      (Right a') -> pure (Right a', universe', messages)
      (Left e) -> do
        let FMT g = h e
        (b, universe'', messages') <- g env universe'
        pure (b, universe'', messages <> messages')
instance (Monad m) => MonadFail (FMT t m) where
  fail msg = FMT $ \_ universe -> pure (Left $ MonadFailing $ T.pack msg, universe, [])
