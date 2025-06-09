{-# LANGUAGE TemplateHaskell #-}

module Lift.Common.Module where

import Data.Text qualified as T
import Extra
import Lift.Common.Names
import Lift.Common.Tokens

data Assertion t = Axiom t | Theorem t | Definition t
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data ParseState t = ParseState
  { _currentModule :: Name
  , _currentFile :: FPath
  , _programState :: Program t
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data Program t = Program
  { _modules :: Map Name (Module t)
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data Module t = Module
  { _moduleName :: Name
  , _tokens :: Map Name (TokenValue t)
  , _assertions :: Map Name t
  , _proofs :: Map Name (Assertion t)
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

makeLenses ''Module
makeLenses ''Program
makeLenses ''ParseState
