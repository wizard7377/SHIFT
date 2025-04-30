{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.Monad where

import Control.Monad
import Control.Monad.Except (Except, ExceptT (ExceptT), MonadError (..), runExceptT)
import Control.Monad.RWS
import Control.Monad.Reader (MonadReader (..), ReaderT)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans as Trans
import Control.Monad.Writer (MonadWriter (..))
import Data.Bifunctor (second)
import Data.Functor ((<&>))
import Data.Functor.Identity (Identity)
import Data.Typeable
import Extra
import Extra.Choice
import Extra.Error (Error)
import Rift (Term, Term')
import Rift qualified
import Sift.Base (LogicEnv, defaultEnv)

{- |
 The central and most general logic monad transformer.
 Takes in a `LogicEnv`, a state `s`, and a monad `m`, and is equipped with a writter `w` and a potential throw
 Approx `r -> s -> w -> m (w, ChoiceT (Either Error a))`
-}
newtype LMT (s :: Type) (w :: Type) (m :: Type -> Type) (a :: Type) = LMT
  { -- unLMT :: ChoiceT (ExceptT Error (RWST LogicEnv w s m)) a
    unLMT :: (ExceptT Error (ChoiceT (RWST LogicEnv w s m))) a
  -- ^ The functional internals
  }

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM s a = LMT s () Identity a

-- | With an environment, and some sentences, generate a state @me@
class EnterState me where
  -- enterState :: (Rift.Sentence sen Term) => LogicEnv -> [sen atom] -> me sen atom
  enterState :: LogicEnv -> [Term' atom] -> me atom

runLMT :: LMT s w m a -> ExceptT Error (ChoiceT (RWST LogicEnv w s m)) a
runLMT = unLMT
mkLMT :: (Monad m) => (ExceptT Error (ChoiceT (RWST LogicEnv w s m))) a -> LMT s w m a
mkLMT = LMT

runLMT' :: (Monad m) => LMT s w m a -> LogicEnv -> s -> m ([Either Error a], s, w)
runLMT' comp = runRWST (runChoiceT (runExceptT (runLMT comp)))
mkLMT' :: (LogicEnv -> s -> m ([Either Error a], s, w)) -> LMT s w m a
mkLMT' comp = LMT $ ExceptT $ Choice $ RWST comp
