{-# LANGUAGE PartialTypeSignatures #-}

module Sift.Ops.Simple where

import Control.Monad.RWS qualified as M
import Extra
import Sift.Ops.Common

nullConvert :: Convert
nullConvert t0 t1 = do
  pure (t0 == t1)
alphaConvert :: _
alphaConvert = _
