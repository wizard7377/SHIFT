{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Lift.Core.Symbol.Types where

import Control.Lens
import Data.Default
import Data.Text qualified as T
import Extra
import Lift.Core.Source.Core
import Lift.Core.Symbol.Typeset
import Text.LaTeX qualified as LaTeX

type Name = T.Text
type ModuleName = [Name]
type SymbolName = Name
data QName = QName ModuleName SymbolName
  deriving (Eq, Ord, Show, Generic, Data)

data Symbol = Symbol
  { _name :: T.Text
  , _sloc :: SourceRange
  , _sdata :: SymbolData
  }
  deriving (Generic)

-- | The type of symbols data, that is, information about the symbol not including it's reperesentation
data SymbolData
  = MathSymbol
      { _arity :: Maybe Int
      , _typetext :: Maybe (Command T.Text)
      , _typelatex :: Maybe (Command LaTeX.LaTeX)
      }
  | ProofSymbol
  | AxiomSymbol
  | LocalAlias Name
  | ForiegnAlias QName
  deriving (Generic)

makeLenses ''QName
makeLenses ''Symbol
makeLenses ''SymbolData

mkSymbol :: T.Text -> SourceRange -> SymbolData -> Symbol
mkSymbol = Symbol
