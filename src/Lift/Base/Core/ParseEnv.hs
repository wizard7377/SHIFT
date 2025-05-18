module Lift.Base.Core.ParseEnv where

data ParseEnv = ParseEnv
  { _cwd :: FilePath
  , _file :: FilePath
  }
