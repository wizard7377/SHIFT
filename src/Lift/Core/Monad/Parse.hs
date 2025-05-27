module Lift.Core.Monad.Parse where

import Control.Monad.RWS
import Data.Text qualified as T
import Extra
import Lift.Core.Forms.Module
import Lift.Core.Monad.Monad
import Lift.Core.Source.Core
import Text.Megaparsec

data LiftError
  = SymbolNotFound
  | SymbolAlreadyExists
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
