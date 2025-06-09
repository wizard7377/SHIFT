{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

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

-- import Sift.Core.Ops.Mem hiding (FTerm')

import Control.Exception (SomeException, try)
import Data.Either.Extra (eitherToMaybe)
import Data.Maybe (maybeToList)
import System.IO.Unsafe (unsafePerformIO)
import Prelude hiding (lex)

justAssume :: Maybe a -> a
justAssume !v = case v of
  Just val -> val
  _ -> error "justAssume: Nothing value encountered"

tRead :: String -> (IO TestTerm)
tRead = parseTerm . T.pack
instance Read (TestTerm) where
  readsPrec _ str = (,) <$> (maybeToList $ eitherToMaybe $ unsafePerformIO $ (try :: IO TestTerm -> IO (Either SomeException TestTerm)) $ tRead str) <*> pure ""
