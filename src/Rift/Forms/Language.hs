{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift.Forms.Language
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Rift.Forms.Language where

import Extra
import Rift.Forms.Theory

data ParseEnv = ParseEnv
  { _cwd :: FilePath
  , _file :: FilePath
  }
makeLenses ''ParseEnv
defaultParseEnv :: ParseEnv
defaultParseEnv = ParseEnv "." "TESTING"
class (Theory (TheoryOf tag)) => Language (tag :: k) where
  type TheoryOf tag
  parseTheory :: ParseEnv -> TheoryOf tag
