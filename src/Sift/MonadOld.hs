{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.MonadOld where

{-
import Control.Monad
import Control.Monad.Except (Except, ExceptT, MonadError (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans as Trans
import Control.Monad.Writer (MonadWriter (..))
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Typeable
import Extra.Choice
import Extra.Error (Error)
import Rift (Term)
import Rift qualified
import Rift.Core.Base (Atomic, Sentence)
import Sift.Base (LogicEnv, defaultEnv)

{- |
 The central and most general logic monad transformer.
 Takes in a `LogicEnv`, a state `s`, and a monad `m`, and is equipped with a writter `w` and a potential throw
-}
newtype LMT s w m a = LMT
  { unLMT :: LogicEnv -> s -> m (w, Choice (Either Error a))
  -- ^ The functional internals
  }

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM s a = LMT s () Identity a

{-
instance (Functor f) => Functor (LMT s f) where
  fmap :: (a -> b) -> LMT s f a -> LMT s f b
  fmap func val = LMT $ \env stateV ->
    let val' = unLMT val env stateV
        ret = fmap func <$> val'
     in ret

instance (Applicative f) => Applicative (LMT s f) where
  pure :: a -> LMT s f a
  pure v = LMT $ \_ _ -> pure $ Right v
  (<*>) :: LMT s f (a -> b) -> LMT s f a -> LMT s f b
  func <*> val = LMT $ \env stateV ->
    let val' = unLMT val env stateV
        func' = unLMT func env stateV
        transf = (\func1 val1 -> case func1 of
          Right func2 -> Right $ func1 val1
          Left e -> e)
    in transf <$> func' <*> val'
-}
instance (Functor f) => Functor (LMT s w f) where
  fmap :: (a -> b) -> LMT s w f a -> LMT s w f b
  fmap func val = LMT $ \env state ->
    let val' = unLMT val env state
     in val'
          <&> second
            ( \case
                Right v -> Right $ func v
                Left e -> Left e
            )

instance (Monoid w, Applicative f) => Applicative (LMT s w f) where
  pure :: a -> LMT s w f a
  pure v = LMT $ \_ _ -> pure (mempty, Right v)
  (<*>) :: LMT s w f (a -> b) -> LMT s w f a -> LMT s w f b
  func <*> val = LMT $ \env stateV ->
    let val' = unLMT val env stateV
        func' = unLMT func env stateV
        transf = (\(w1, func1) (w2, val1) -> (w1 <> w2, func1 <*> val1))
     in transf <$> func' <*> val'

instance (Monoid w, Monad m) => Monad (LMT s w m) where
  return :: a -> LMT s w m a
  return = pure
  (>>=) :: LMT s w m a -> (a -> LMT s w m b) -> LMT s w m b
  v >>= c = LMT $ \env stateV -> do
    (w1, res1) <- unLMT v env stateV
    case res1 of
      Left e -> return (w1, Left e)
      Right v1 ->
        unLMT (c v1) env stateV
          <&> second
            ( \case
                Left e -> Left e
                Right v2 -> Right v2
            )

instance (Monoid w) => Trans.MonadTrans (LMT s w) where
  lift :: (Monad m) => m a -> LMT s w m a
  lift inner = LMT $ \_ _ -> (mempty,) . Right <$> inner

-- | Get the environment
instance (Monoid w, Monad m) => MonadReader LogicEnv (LMT s w m) where
  ask :: LMT s w m LogicEnv
  ask = LMT $ \env _ -> return (mempty, Right env)
  local :: (LogicEnv -> LogicEnv) -> LMT s w m a -> LMT s w m a
  local trans param = LMT $ \env stateV -> unLMT param (trans env) stateV

-- | Get and set the state
instance (Monoid w, Monad m) => MonadState s (LMT s w m) where
  state :: (s -> (a, s)) -> LMT s w m a
  state trans = LMT $ \_ stateV -> (let (res, _) = trans stateV in return (mempty, Right res))

instance (Monoid w, Monad m) => MonadWriter w (LMT s w m) where
  tell :: w -> LMT s w m ()
  tell w = LMT $ \_ _ -> return (w, Right ())
  pass :: LMT s w m (a, w -> w) -> LMT s w m a
  pass = _
  listen :: LMT s w m a -> LMT s w m (a, w)
  listen input = _
instance (Monoid w, Monad m) => MonadError Error (LMT s w m) where
  throwError :: Error -> LMT s w m a
  throwError e = LMT $ \_ _ -> return (mempty, Left e)
  catchError :: LMT s w m a -> (Error -> LMT s w m a) -> LMT s w m a
  catchError f c = LMT $ \env stateV -> do
    (w1, res1) <- unLMT f env stateV
    case res1 of
      Left e -> unLMT (c e) env stateV
      Right v -> return (w1, Right v)

class EnterState term state where
  enterState :: LogicEnv -> [term] -> state
testLMT :: (Monad m) => (Monoid w) => (EnterState (Term' atom) s) => LMT s w m a -> [Term' atom] -> m (w, Either Error a)
testLMT comp states = runLMT comp defaultEnv (enterState defaultEnv states)
runLMT :: (Monad m, Monoid w) => LMT s w m a -> LogicEnv -> s -> m (w, Either Error a)
runLMT = unLMT
mkLMT :: (Monoid w, Monad m) => (LogicEnv -> s -> m (w, Either Error a)) -> LMT s w m a
mkLMT = LMT
-}
