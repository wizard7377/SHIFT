{-# LANGUAGE TemplateHaskell #-}

module Lift.Base.Core.Symbol.SymbolTable where

import Control.Lens
import Data.Map
import Data.Map qualified as Map
import Extra
import Lift.Base.Core.Source.Core
import Lift.Base.Core.Symbol.Types (ModuleName, Name, QName, Symbol)

-- | The type of symbol tables, one of which is associated with each module
data SymbolTable = SymbolTable
  { _name :: ModuleName
  -- ^ Included for redundancy, this is the module name
  , _source :: SourceRange
  -- ^ Where the module was defined
  , _symbols :: Map.Map Name Symbol
  }
  deriving (Generic)

makeLenses ''SymbolTable
