{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.Parse where

import Data.Text qualified as T
import Extra
import Rift.Forms.Theory (Theory (..))
import Text.Megaparsec qualified as P

class (Theory t, Monad (MonadOf t)) => UserTheory t where
  type MonadOf t :: Type -> Type
  type MonadOf t = IO
  parseTerm :: T.Text -> (MonadOf m) (TermOf t)
  printTerm :: (TermOf t) -> (MonadOf m) (T.Text)
