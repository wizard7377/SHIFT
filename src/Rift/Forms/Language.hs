{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Forms.Language where

import Rift.Forms.Theory

data ParseEnv = ParseEnv
  { _cwd :: FilePath
  , _file :: FilePath
  }
class (Theory (TheoryOf tag)) => Language (tag :: k) where
  type TheoryOf tag
  parseTheory :: ParseEnv -> TheoryOf tag
