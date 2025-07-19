{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Rift
Description : Representation for Intutionsitic Flat Types
License     : BSD-2-Clause
Maintainer  : Asher Frost

The abstract representation and syntax for the SHIFT ecosystem.
This module re-exports the core and forms modules of the RIFT library,
providing the main term types and constructors for use throughout SHIFT.

= Overview

The RIFT system provides a representation for Intuitionistic Flat Types,
including the core term data structures and forms for communication and proofs.
This module is the main entry point for working with RIFT terms in the SHIFT ecosystem.

== Exports

[@module Rift.Core@]    The core forms of the RIFT library, e.g., terms
[@module Rift.Forms@]   The communication forms, e.g., proofs
[@Term'@]               The main term type parameterized by atom type
-}
module Rift (
  -- | The core forms of the RIFT library, for instance terms
  module Rift.Core,
  -- | The communication terms of the RIFT library, for instance proofs
  module Rift.Forms,
) where

-- \|
--    The representation for Flat Types
import Rift.Core
import Rift.Forms

-- ^ Flat Intermediate Representation
