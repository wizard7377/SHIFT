{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.Choice.Types where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Cont
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.List (singleton)
import Data.List.Extra (nubBy)
import Data.Maybe
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra ((<&>))
import Extra.Basics
import Extra.List
import Extra.Ops

-- The monadic list type
data MStep m a = MNil | MCons a (MList m a)
type MList m a = m (MStep m a)

-- This can be directly used as a monad transformer
newtype ChoiceT m a = ChoiceT {_runChoiceT :: MList m a}
type Choice = ChoiceT Identity

class MonadChoice (m :: Type -> Type) (a :: Type) where
  csplit :: (Foldable f) => m (f a) -> m a

cguard :: (Alternative m) => Bool -> m ()
cguard True = pure ()
cguard False = empty
