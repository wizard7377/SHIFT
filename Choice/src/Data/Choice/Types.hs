{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Data.Choice.Types where

import Control.Applicative
import Control.Arrow qualified as A
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Coerce
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.Kind (Type)
import Data.List (singleton)
import Data.Maybe

-- The monadic list type
data StepT m a where
  MNil :: StepT m a
  MCons :: a -> (ListT m a) -> StepT m a
type ListT m a = m (StepT m a)
type Yield m a = m (Maybe (a, m a))

yield :: (Monad m) => a -> m a -> Yield m a
yield a next = pure (Just (a, next))

-- This can be directly used as a monad transformer
newtype ChoiceT m a = ChoiceT {_runChoiceT :: ListT m a}
type Choice = ChoiceT Data.Functor.Identity.Identity

{- | The class of Monads that are non-deterministic
Features two types, a given "viewed" type and a given "choice" type
Note that this is not one-to-one, one can view one choice as many things, and one view can see many choices
-}
class (Monad m) => MonadChoice (m :: Type -> Type) where
  cset :: m (Maybe (a, m a)) -> m a
  cget :: m a -> m (Maybe (a, m a))

type ChoiceA m = A.Kleisli (ChoiceT m)
