module Extra.Choice.Pretty where

import Control.Comonad (Comonad (..))
import Extra.Choice.Core
import Extra.Choice.Instances
import Extra.Choice.Interface
import Extra.Choice.Types

prettyChoice :: (Show a, Monad m) => ChoiceT m a -> m String
prettyChoice choiceT = do
  choices <- execChoiceT choiceT
  return $ unlines $ map show choices

instance (Show a, Monad m, Comonad m) => Show (ChoiceT m a) where
  show choiceT = extract $ prettyChoice choiceT
