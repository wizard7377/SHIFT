{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Search
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Search (LogicEnv (..), Search (..), logicEnvDepth) where

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
defaultDepth = 4
defaultUnfold :: Int
defaultUnfold = 4

instance Default (LogicEnv) where
  def = LogicEnv defaultDepth defaultUnfold

type Search e a r = (Theory a) => LogicEnv -> e -> a -> a -> r
makeLenses ''LogicEnv
