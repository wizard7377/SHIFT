{-# LANGUAGE GHC2021 #-}
-- \|
-- Module      : Rift
-- Description : Representation for Intutionsitic Flat Types
-- License     : BSD-2-Clause
-- Maintainer  : Asher Frost
--
-- The abstract representation and syntax for the SHIFT ecosystem
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

module Rift (
  -- | The core forms of the RIFT library, for instance terms
  module Rift.Core,
  -- | The communication terms of the RIFT library, for instance proofs
  module Rift.Forms,
  Term' (..),
)
where

-- \|
--    The representation for Flat Types
import Rift.Core
import Rift.Forms

-- ^ Flat Intermediate Representation
