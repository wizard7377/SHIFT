module Lift.Base.Core.Monad.Parse where

import Control.Monad.RWS
import Data.Text qualified as T
import Extra
import Lift.Base.Core.Forms.Module
import Lift.Base.Core.Monad.Monad
import Lift.Base.Core.Source.Core
import Text.Megaparsec

data LiftError
  = SymbolNotFound
  deriving (Eq, Show, Ord, Generic, Data)
type PFMT t m = ParsecT LiftError T.Text (FMT t m)

withPos :: (Monad m) => PFMT t m a -> PFMT t m (a, SourceRange)
withPos p = do
  state <- get
  let cfile = state ^. currentFile
  start <- getSourcePos
  res <- p
  end <- getSourcePos
  return (res, SourceRange start end)
