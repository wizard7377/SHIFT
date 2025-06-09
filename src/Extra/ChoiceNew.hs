{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE UndecidableInstances #-}

-- {-# LANGUAGE UndecidableInstances #-}

module Extra.ChoiceNew where

import Control.Applicative
import Control.Comonad
import Control.Monad
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState)
import Control.Monad.RWS.Class (MonadReader (..), MonadState (..), MonadWriter (..))
import Control.Monad.Trans
import Data.Foldable (Foldable (..))
import Data.Functor.Identity (Identity (..))
import Data.List (singleton)
import Data.List.Extra (nubBy)
import Data.Maybe (catMaybes, isJust, mapMaybe)
import Extra.Basics
import Extra.List
import Extra.Ops

data ChoiceT m a where
  CAbsurd :: ChoiceT m a
  CYield :: m (a, ChoiceT m a) -> ChoiceT m a

choiceOr :: ChoiceT m a -> ChoiceT m a -> ChoiceT m a
choiceOr choiceA choiceB =
  case choiceB of
    CAbsurd -> choiceA
    CYield f -> case choiceA of
      CAbsurd -> choiceB
      CYield _ -> do
        (y, rest) <- f
        g <- (y,) <$> (choiceOr choiceA rest)
        _
