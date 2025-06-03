{-# LANGUAGE LambdaCase #-}

module Sift.Core.Dev.Util where

import Control.Lens ((^.), _2)
import Control.Monad.State
import Data.List.Extra (splitOn)
import Data.Text qualified as T
import Extra
import Extra.TestHelp (makeTestList)
import Rift.Core.Base
import Rift.Core.Interface (FTerm' (..))
import Sift.Core.Dev.Parser
import Sift.Core.Ops.Mem hiding (FTerm')
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (MonadParsec (..), parseMaybe, parseTest, runParserT)
import Text.Megaparsec.Error (errorBundlePretty)
import Prelude hiding (lex)

justAssume :: Maybe a -> a
justAssume !v = case v of
  Just val -> val
  _ -> error "justAssume: Nothing value encountered"

tRead :: String -> Maybe TestTerm
tRead input = (parseTerm input)
instance Read (TestTerm) where
  readsPrec _ str =
    case (tRead str) of
      Just x -> [(x, "")]
      Nothing -> []
