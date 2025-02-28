{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.Monad where

import Control.Monad
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans as Trans
import Data.Functor.Identity (Identity)
import Data.Typeable
import Rift (Term)
import Rift qualified
import Sift.Base (LogicEnv)

-- | The basic monad transformer, a computation over a monad with `LogicEnv` and some state @s@, to produce an action @a@
newtype LMT s m a = LMT
  { unLMT :: LogicEnv -> s -> m a
  -- ^ The functional internals
  }

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM s a = LMT s Identity a

type LMT' s m a = LMT m s a

instance (Functor f) => Functor (LMT s f) where
  fmap :: (a -> b) -> LMT s f a -> LMT s f b
  fmap func val = LMT $ \env stateV ->
    let val' = unLMT val env stateV
        ret = fmap func val'
     in ret

instance (Applicative f) => Applicative (LMT s f) where
  pure :: a -> LMT s f a
  pure v = LMT $ \_ _ -> pure v
  (<*>) :: LMT s f (a -> b) -> LMT s f a -> LMT s f b
  func <*> val = LMT $ \env stateV ->
    let val' = unLMT val env stateV
        func' = unLMT func env stateV
        ret = func' <*> val'
     in ret

instance (Monad m) => Monad (LMT s m) where
  return :: a -> LMT s m a
  return = pure
  (>>=) :: LMT s m a -> (a -> LMT s m b) -> LMT s m b
  v >>= c = LMT $ \stateV env ->
    ( let r0 = (unLMT v) stateV env
          r1 = (lift r0) >>= c
          r2 = (unLMT r1) stateV env
       in r2
    )

instance Trans.MonadTrans (LMT s) where
  lift :: m a -> LMT s m a
  lift inner = LMT $ \_ _ -> inner

-- | Get the environment
instance (Monad m) => MonadReader LogicEnv (LMT s m) where
  ask :: LMT s m LogicEnv
  ask = LMT $ \env _ -> return env
  local :: (LogicEnv -> LogicEnv) -> LMT s m a -> LMT s m a
  local trans param = LMT $ \env stateV -> unLMT param (trans env) stateV

-- | Get and set the state
instance (Monad m) => MonadState s (LMT s m) where
  state :: (s -> (a, s)) -> LMT s m a
  state trans = LMT $ \_ stateV -> (let (res, _) = trans stateV in return res)

-- | With an environment, and some sentences, generate a state @me@
class EnterState me where
  enterState :: (Rift.Sentence sen) => LogicEnv -> [sen atom] -> me sen atom

runLMT :: LMT s m a -> LogicEnv -> s -> m a
runLMT = unLMT
mkLMT :: (LogicEnv -> s -> m a) -> LMT s m a
mkLMT = LMT
