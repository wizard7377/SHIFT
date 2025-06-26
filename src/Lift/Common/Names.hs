module Lift.Common.Names where

import Control.Lens qualified as Lens
import Data.Text qualified as T
import Extra
import Text.Megaparsec qualified as P

data Name = Name T.Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data QPath = QPath [Name]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data QName = QName QPath Name
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data FPath = FPath FilePath
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
data Offset = Offset
  { _offsetLine :: Int
  , _offsetColumn :: Int
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data FPos = FPos
  { _filePath :: FPath
  , _startOffset :: Offset
  , _endOffset :: Offset
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

fromPos :: P.SourcePos -> P.SourcePos -> FPos
fromPos pos pos1 =
  FPos
    { _filePath = FPath (P.sourceName pos)
    , _startOffset = Offset (P.unPos (P.sourceLine pos)) (P.unPos (P.sourceColumn pos))
    , _endOffset = Offset (P.unPos (P.sourceLine pos1)) (P.unPos (P.sourceColumn pos1))
    }

nameText :: Lens.Iso' Name T.Text
nameText = Lens.iso (\(Name n) -> n) Name
toName n = Name n
fromName (Name n) = n

toFilePath :: FPath -> FilePath
toFilePath (FPath fp) = fp
fromFilePath fp = FPath fp
