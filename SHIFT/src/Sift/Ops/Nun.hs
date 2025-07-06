{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Nun (
  nunReduce,
)
where

import Extra
import Rift qualified
import Sift.Core.Monad

nunConvert :: Convert t e
nunConvert = _
