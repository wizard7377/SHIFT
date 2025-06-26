{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Extra.Choice.Exports
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Extra.Choice.Exports (
  -- * The core monads
  -- $monads
  Choice,
  ChoiceT,
  StepT (..),
  CBool,

  -- ** Associated functions
  runChoice,
  execChoiceT,
  liftStep,
  -- HACK: The following code should be replaced by a method in `MonadChoice`
  fromList,
  ctoList,

  -- * Combinators
  coptional,
  cguard,
  cnot,
  csingle,
  cand,
  liftChoice,
  cifte,
  recover,
  cguardM,
) where

-- \$monads
-- These are the core monads for the `Extra.Choice` library.
--
-- There are two broad approaches to encoding "list monad transformers", but first, the two bad ways:
-- 1. @type ListT m a = m [a]@. Technically, this /is/ a monad transformer, just not a very interesting one

import Extra.Choice.Combinators
import Extra.Choice.Core
import Extra.Choice.Instances
import Extra.Choice.Interface
import Extra.Choice.Pretty
import Extra.Choice.Types
