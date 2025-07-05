{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Language
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Language where

import Data.Text qualified as T
import Extra
import Rift.Forms.Theory

data ParseEnv = ParseEnv
  { _cwd :: FilePath
  }
makeLenses ''ParseEnv
defaultParseEnv :: ParseEnv
defaultParseEnv = ParseEnv "."

-- | The class of all languages that can be parsed
class (Monad m) => Language m l where
  type ResultOfL l :: Type
  type ResultOfLT l :: Type
  type ResultOfLT l = ResultOfL l

  -- | Given a file name, parse the file and return the result in a monad
  parseFile :: l -> ParseEnv -> T.Text -> m (ResultOfL l)

  -- | Given some scratch text, parse it and return the result in the monad
  parseText :: l -> ParseEnv -> T.Text -> m (ResultOfLT l)

newtype ALanguage m = ALanguage (forall l. (Language m l) => l)
