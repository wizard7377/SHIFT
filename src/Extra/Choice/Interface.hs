{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Extra.Choice.Interface (runChoice, runChoiceT) where

import Control.Applicative
import Control.Monad.Identity
import Extra.Choice.Core
import Extra.Choice.Instances
import Extra.Choice.Types

runChoiceT = _
runChoice :: (Alternative m) => ChoiceT Identity a -> m a
runChoice (ChoiceT v) = runList v
runList :: (Alternative m) => MList Identity a -> m a
runList v =
  case v' of
    MNil -> empty
    MCons a b -> pure a <|> runList b
 where
  v' = runIdentity v
