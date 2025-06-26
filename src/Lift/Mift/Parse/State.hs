{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Mift.Parse.State
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Mift.Parse.State where

import Control.Lens (from, re, (%=), (<>=))
import Control.Lens.Combinators (to)
import Control.Lens.Operators ((^..))
import Control.Monad.Except qualified as M
import Control.Monad.Extra
import Control.Monad.RWS qualified as M
import Extra
import Lift.Common.Module (Module, assertions, currentModule, modules, programState, proofs, tokens)
import Lift.Common.Names
import Lift.Common.Tokens
import Lift.Mift.Base
import Lift.Mift.Expr (MiftExpr)

mkWith :: (k -> Lens' (Module MiftExpr) [v]) -> k -> v -> MiftM ()
mkWith with name value = do
  state <- M.get
  let symbol = state ^.. programState . modules . mAt (state ^. programState . currentModule) . each . mto
  if (null (symbol ^. (each . with name)))
    then do
      let Just cmod = state ^? (programState . modules . mAt (state ^. programState . currentModule) . each . mto)
      let mod1 = cmod & (with name) .~ [value]
      (programState . modules . mAt (state ^. programState . currentModule) . each . mto) .= mod1
    else
      _
getWith :: (k -> Lens' (Module MiftExpr) [v]) -> k -> MiftM v
getWith with name = do
  state <- M.get
  let symbol = state ^.. programState . modules . mAt (state ^. programState . currentModule) . each . mto
  case symbol ^? (each . with name) of
    Just (x : _) -> pure x
    _ -> _

mkToken :: Name -> TImage () Name (TokenValue MiftExpr) -> MiftM ()
mkToken = mkWith (\x -> tokens . mAt x)
getToken :: Name -> MiftM (TImage () Name (TokenValue MiftExpr))
getToken = getWith (\x -> tokens . mAt x)
mkProof :: MiftExpr -> TImage Name MiftExpr MiftExpr -> MiftM ()
mkProof = mkWith (\x -> proofs . mAt x)
getProof :: MiftExpr -> MiftM (TImage Name MiftExpr MiftExpr)
getProof = getWith (\x -> proofs . mAt x)
mkAssert :: MiftExpr -> TImage Name MiftExpr MiftExpr -> MiftM ()
mkAssert = mkWith (\x -> assertions . mAt x)
getAssert :: MiftExpr -> MiftM (TImage Name MiftExpr MiftExpr)
getAssert = getWith (\x -> assertions . mAt x)
