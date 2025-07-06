module Data.Choice.Pretty where

import Control.Comonad (Comonad (..))
import Data.Choice.Core
import Data.Choice.Instances
import Data.Choice.Interface
import Data.Choice.Types

prettyChoice :: (Show a, Monad m) => ChoiceT m a -> m String
prettyChoice choiceT = do
  choices <- execChoiceT choiceT
  return $ unlines $ map show choices

instance (Show a, Monad m, Comonad m) => Show (ChoiceT m a) where
  show choiceT = extract $ prettyChoice choiceT
