{-# LANGUAGE GHC2021 #-}
-- \|
-- Module      : Sift
-- Description : Solver for Intutioninistic Flat Theorems
-- License     : BSD-2-Clause
-- Maintainer  : Asher Frost
--
-- The primary computational part of the SHIFT ecosystem
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Sift (
  module Sift.Core,
  module Sift.Base,
  module Sift.Ops,
  module Sift.Search,
) where

import Sift.Base
import Sift.Core
import Sift.Ops
import Sift.Search
