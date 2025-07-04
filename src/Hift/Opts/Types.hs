{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Hift.Opts.Types
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Hift.Opts.Types where

import Extra
import Options.Applicative
import Rift qualified

data PathOption = PathOption
  { _file :: FilePath
  , _namespace :: Maybe String
  }
  deriving (Eq, Show, Ord, Data, Typeable, Generic)
data ProgOpts = ProgOpts
  { _inputs :: String
  , _language :: String
  , _action :: Action
  }
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

data ReplOptions = ReplOptions {}
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

data CheckOptions = CheckOptions {}
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

data BuildOptions = BuildOptions {}
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

data TypesetOptions = TypesetOptions {}
  deriving (Eq, Show, Ord, Data, Typeable, Generic)
data Action
  = Repl ReplOptions
  | Check CheckOptions
  | Build BuildOptions
  | Typeset TypesetOptions
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

makeLenses ''ProgOpts
