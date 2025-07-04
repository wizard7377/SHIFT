{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Core.Types
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Core.Types where

import Extra
import Rift.Forms qualified as Rift

data OpTypes
  = AlphaConvert
  | Simple
  | MemRedux
  | ZetaRedux
  deriving (Eq, Ord, Data, Typeable, Generic, Bounded, Enum, Show)
data OpEnv t = OpEnv
  { _logicEnv :: Rift.LogicEnv
  , _opGoal :: Maybe t
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data OpState t = OpState
  { _curDepth :: Int
  , _curGoal :: Maybe t
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data OpAccum t = OpAccum
  { _opHistory :: [OpTypes]
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''OpState
makeLenses ''OpAccum
makeLenses ''OpEnv
instance Semigroup (OpAccum t) where
  OpAccum h1 <> OpAccum h2 = OpAccum (h1 ++ h2)

instance Monoid (OpAccum t) where
  mempty = OpAccum []
instance Default (OpAccum t) where
  def = OpAccum{_opHistory = []}
instance Default (OpEnv t) where
  def = OpEnv{_logicEnv = def, _opGoal = Nothing}

opEnvToState :: OpEnv t -> OpState t
opEnvToState OpEnv{_logicEnv, _opGoal} =
  OpState
    { _curDepth = Rift._logicEnvDepth _logicEnv
    , _curGoal = _opGoal
    }
