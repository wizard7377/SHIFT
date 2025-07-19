{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift.Ops.Nun (

)
where

import Control.Applicative (Alternative (..))
import Extra
import Rift qualified
import Sift.Core.Monad

-- TODO:
nunConvert :: Convert e
nunConvert _ _ = empty
