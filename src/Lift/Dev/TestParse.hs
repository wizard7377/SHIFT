module Lift.Dev.TestParse where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.RWS qualified as M
import Data.Text qualified as T
import Extra
import Lift.Common.Module (ParseState (..), Program (..))
import Lift.Common.Names (FPath (..), fromFilePath, toFilePath, toName)
import Lift.Common.Parsing
import Rift.Forms.Language qualified as Rift
import System.Directory.Extra (getCurrentDirectory)
import Text.Megaparsec qualified as P

-- | Parse in file
testParse :: (MonadIO m, P.ShowErrorComponent e) => (Ord e) => (Monad m) => ParseMT e t m a -> String -> m a
testParse parser file = do
  fileContent <- T.pack <$> (liftIO $ readFile file)
  let result0 = P.runParserT parser file fileContent
  cwd <- liftIO getCurrentDirectory
  (result, state, writer) <-
    M.runRWST
      result0
      (Rift.ParseEnv cwd file)
      ( ParseState
          (FPath (file))
          def
      )
  case result of
    Left err -> do
      liftIO $ putStrLn $ P.errorBundlePretty err
      error "Parse error"
    Right result -> return result
