{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Short
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Short (
  TO,
  TC,
  TOC,
) where

import Data.Kind
import Rift qualified

-- | /T/erm /O/f, alias of 'Rift.TermOf'.
type TO e = Rift.TermOf e

-- | /T/erm /C/lass, alias of 'Rift.Term'.
type TC t = Rift.Term t

-- | /T/erm /O/f /C/lass
type TOC e = (Rift.Theory e, TC (TO e))
