{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Core.Types
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Core.Types where

import Data.Text qualified as T
import Extra
import Rift.Forms qualified as Rift

data OpTypes
  = AlphaConvert
  | Simple
  | MemRedux
  | ZetaRedux
  | FixRedux
  deriving (Eq, Ord, Data, Typeable, Generic, Show)

data OpEnv e = OpEnv
  { _logicEnv :: Rift.LogicEnv
  , _opTheory :: e
  }
  deriving (Typeable)

data OpState = OpState
  { _curDepth :: Int
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data LogArgs = LogArgs T.Text T.Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data OpAccum = OpAccum
  { _opHistory :: [(OpTypes, T.Text)]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''OpState
makeLenses ''OpAccum
makeLenses ''OpEnv
instance Semigroup (OpAccum) where
  OpAccum h1 <> OpAccum h2 = OpAccum (h1 ++ h2)

instance Monoid (OpAccum) where
  mempty = OpAccum []
instance Default (OpAccum) where
  def = OpAccum{_opHistory = []}
instance (Default e) => Default (OpEnv e) where
  def = OpEnv{_logicEnv = def, _opTheory = def}

opEnvToState :: OpEnv e -> OpState
opEnvToState OpEnv{_logicEnv} =
  OpState
    { _curDepth = Rift._logicEnvDepth _logicEnv
    }
