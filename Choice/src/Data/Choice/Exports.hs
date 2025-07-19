{-# LANGUAGE GHC2021 #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Data.Choice.Exports
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Data.Choice.Exports (
  -- ** The core monads
  -- $monads
  Choice,
  ChoiceT,
  StepT (..),
  ChoiceA (..),
  MonadChoice (..),
  Yield,

  -- *** Associated functions
  runChoice,
  runChoiceT,
  execChoiceT,
  liftStep,
  -- HACK: The following code should be replaced by a method in `MonadChoice`
  fromList,
  ctoList,

  -- ** Combinators
  coptional,
  cguard,
  cnot,
  csingle,
  cand,
  liftChoice,
  cifte,
  recover,
  cguardM,
  csplit,
  cfilter,
  (|.|),

  -- *** Re-exports
  Alternative (..),
) where

-- \$monads
-- These are the core monads for the `Data.Choice` library.
--
-- There are two broad approaches to encoding "list monad transformers", but first, the two bad ways:
-- 1. @type ListT m a = m [a]@. Technically, this /is/ a monad transformer, just not a very interesting one

import Control.Applicative (Alternative (some))
import Data.Choice.Combinators
import Data.Choice.Core
import Data.Choice.Instances ()
import Data.Choice.Interface
import Data.Choice.Pretty
import Data.Choice.Types
