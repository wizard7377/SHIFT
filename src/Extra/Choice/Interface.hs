{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Extra.Choice.Interface (runChoice, execChoiceT, liftStep, liftChoice) where

import Control.Applicative
import Control.Monad.Identity
import Control.Monad.Trans
import Extra.Choice.Core
import Extra.Choice.Instances
import Extra.Choice.Types

liftStep :: (Monad m) => (StepT m a -> b) -> (ChoiceT m a -> m b)
liftStep f (ChoiceT x) = f <$> x
execChoiceT :: (Monad m) => ChoiceT m a -> m [a]
execChoiceT (ChoiceT v) = execListT v
runChoice :: (Alternative m) => ChoiceT Identity a -> m a
runChoice (ChoiceT v) = runList v
runList :: (Alternative m) => ListT Identity a -> m a
runList v =
  case v' of
    MNil -> empty
    MCons a b -> pure a <|> runList b
 where
  v' = runIdentity v

makeSimpleChoice :: (Monad m) => [a] -> ChoiceT m a
makeSimpleChoice xs = ChoiceT $ pure $ foldr (\x acc -> MCons x $ pure acc) MNil xs
liftChoice :: (Monad m) => ChoiceT Identity a -> ChoiceT (m) a
liftChoice v =
  let
    v0 = execChoiceT v
    v1 = runIdentity v0
    v2 = makeSimpleChoice v1
   in
    v2
