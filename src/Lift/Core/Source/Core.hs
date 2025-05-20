module Lift.Core.Source.Core (Offset, SourceRange (..), module Text.Megaparsec.Pos) where

import Extra
import Text.Megaparsec.Pos

type Offset = Pos

data SourceRange = SourceRange
  { _start :: SourcePos
  , _end :: SourcePos
  }
  deriving (Eq, Ord, Show, Generic, Data)
