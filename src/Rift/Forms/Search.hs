{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Search
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Search where

import Control.Lens (makeLenses)
import Data.Default
import Extra

data LogicEnv t = LogicEnv
  { _logicEnvTheory :: t
  , _logicEnvDepth :: Int
  , _logicEnvUnfold :: Int
  }
defaultDepth :: Int
defaultDepth = 2
defaultUnfold :: Int
defaultUnfold = 4
