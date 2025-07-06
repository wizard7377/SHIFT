{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Tift.Types
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Tift.Types where

import Control.Monad.State
import Data.Text qualified as T
import Data.Void
import Extra
import Lift.Tift.Expr
import Rift qualified
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

data ParseState t = ParseState
  { _cindex :: Int
  , _theory :: Rift.SimpleTheory () T.Text t
  , _unnamedScope :: Int
  , _namedScopes :: Map T.Text Int
  }

makeLenses ''ParseState
instance Default (ParseState t) where
  def = ParseState 0 (Rift.SimpleTheory mempty mempty def) 1 mempty
type Parser' t = ParsecT Void T.Text (State (ParseState t))
type Parser = Parser' TiftTerm
