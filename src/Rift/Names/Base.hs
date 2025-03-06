module Rift.Names.Base where

import Data.Tree qualified

type Todo = ()
type NamePath = [Int]
data RelativeName = RelativeName
  { _level :: Int
  , _pos :: NamePath
  }
  deriving (Show, Eq)

data Symbol = Token Todo | Alias RelativeName
  deriving (Show, Eq)

type SymbolTable = Data.Tree.Tree Symbol

data SymbolToken = SymbolToken
  { _name :: NamePath
  , _from :: Todo
  }
