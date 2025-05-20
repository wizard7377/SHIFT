{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lift.Core.Forms.Module where

import Control.Lens (At, Index, IxValue, Ixed, ilens, lens, makeLenses, (&))
import Control.Lens.Combinators (At (..), ix)
import Control.Lens.Lens (IndexedLens')
import Data.Data
import Data.Map qualified as Map
import Extra
import Lift.Core.Forms.Axiom
import Lift.Core.Forms.Proof
import Lift.Core.Source.Core
import Lift.Core.Symbol.SymbolTable
import Lift.Core.Symbol.Types

data Module t = Module
  { _moduleName :: ModuleName
  , _moduleSource :: SourceRange
  , _axioms :: Map.Map SymbolName (Statement t)
  , _theorems :: Map.Map SymbolName (Statement t)
  , _proofs :: Map.Map SymbolName (Proof t)
  , _symboltable :: SymbolTable
  , _queries :: Todo
  }
  deriving (Generic)
data Universe t = Universe {_modmap :: Map.Map ModuleName (Module t)}

-- | A universe is a collection of modules indexed by name
data LiftState t = LiftState
  { _universe :: Universe t
  , _currentModule :: ModuleName
  , _currentFile :: FilePath
  }

makeLenses ''Universe
makeLenses ''LiftState
makeLenses ''Module
type instance Index (Universe t) = ModuleName
type instance IxValue (Universe t) = Module t
instance Ixed (Universe t) where
  ix qname = (modmap . ix qname)
instance At (Universe t) where
  at qname = (modmap . at qname)
