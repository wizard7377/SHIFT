{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Search
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Search (LogicEnv (..), Search (..)) where

import Control.Lens (makeLenses)
import Data.Default
import Extra
import Rift.Forms.Theory (Theory)

data LogicEnv = LogicEnv
  { _logicEnvDepth :: Int
  , _logicEnvUnfold :: Int
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
defaultDepth :: Int
defaultDepth = 2
defaultUnfold :: Int
defaultUnfold = 4

instance Default (LogicEnv) where
  def = LogicEnv defaultDepth defaultUnfold

-- | The core search class
class (Monad m) => Search m t s where
  type ResultOfS s
  search :: s -> LogicEnv -> t -> m (ResultOfS s)

newtype ASearch m t = ASearch (forall s. (Search m t s) => s)
