{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Extra.Choice.Interface (runChoice, runChoiceT, execChoiceT, liftStep) where

import Control.Applicative
import Control.Monad.Identity
import Extra.Choice.Core
import Extra.Choice.Instances
import Extra.Choice.Types

liftStep :: (Monad m) => (StepT m a -> b) -> (ChoiceT m a -> m b)
liftStep f (ChoiceT x) = f <$> x
execChoiceT :: (Monad m) => ChoiceT m a -> m [a]
execChoiceT (ChoiceT v) = execListT v
runChoiceT = _
runChoice :: (Alternative m) => ChoiceT Identity a -> m a
runChoice (ChoiceT v) = runList v
runList :: (Alternative m) => ListT Identity a -> m a
runList v =
  case v' of
    MNil -> empty
    MCons a b -> pure a <|> runList b
 where
  v' = runIdentity v
