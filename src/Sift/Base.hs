{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Base
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Base where

import Control.Applicative (Alternative (empty, (<|>)))
import Data.Kind (Type)
import Rift
import Rift qualified
