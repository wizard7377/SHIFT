{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-noncanonical-monad-instances #-}

module Sift.MonadNew where

import Control.Monad
import Control.Monad.Except (Except, ExceptT, MonadError (..))
import Control.Monad.RWS
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
import Rift (Term, Term')
import Rift qualified
import Sift.Base (LogicEnv, defaultEnv)

{- |
 The central and most general logic monad transformer.
 Takes in a `LogicEnv`, a state `s`, and a monad `m`, and is equipped with a writter `w` and a potential throw
-}
newtype LMT s w m a = LMT
  { unLMT :: RWST LogicEnv w s (ExceptT Error m) (Choice a)
  -- ^ The functional internals
  }

-- | A specification of `LMT` as a regular monad, simply over `Identity`
type LM s a = LMT s () Identity a

-- | With an environment, and some sentences, generate a state @me@
class EnterState me where
  -- enterState :: (Rift.Sentence sen Term) => LogicEnv -> [sen atom] -> me sen atom
  enterState :: LogicEnv -> [Term' atom] -> me atom

testLMT :: (Monoid w, Monad m, EnterState s) => LMT (s atom) w m a -> [Term' atom] -> m (w, Either Error a)
testLMT comp sens = runLMT comp defaultEnv (enterState defaultEnv sens)
runLMT :: (Monad m, Monoid w) => LMT s w m a -> LogicEnv -> s -> m (w, Either Error a)
runLMT = unLMT
mkLMT :: (Monoid w, Monad m) => (LogicEnv -> s -> m (w, Either Error a)) -> LMT s w m a
mkLMT = LMT
