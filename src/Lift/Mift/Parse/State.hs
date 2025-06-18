module Lift.Mift.Parse.State where

import Control.Lens (from, re, (%=), (<>=))
import Control.Lens.Combinators (to)
import Control.Lens.Operators ((^..))
import Control.Monad.Except qualified as M
import Control.Monad.Extra
import Control.Monad.RWS qualified as M
import Extra
import Lift.Common.Module (Module, assertions, currentModule, moduleName, modules, programState, proofs, tokens)
import Lift.Common.Names
import Lift.Common.Tokens
import Lift.Mift.Base
import Lift.Mift.Expr (MiftExpr)

mkWith :: (Name -> Lens' (Module MiftExpr) [v]) -> Name -> v -> MiftM ()
mkWith with name value = do
  state <- M.get
  let symbol = state ^.. programState . modules . mAt (state ^. currentModule) . each . mto
  if (null (symbol ^. (each . with name)))
    then do
      let Just cmod = state ^? (programState . modules . mAt (state ^. currentModule) . each . mto)
      let mod1 = cmod & (with name) .~ [value]
      (programState . modules . mAt (state ^. currentModule) . each . mto) .= mod1
    else
      _
getWith :: (Name -> Lens' (Module MiftExpr) [v]) -> Name -> MiftM v
getWith with name = do
  state <- M.get
  let symbol = state ^.. programState . modules . mAt (state ^. currentModule) . each . mto
  case symbol ^? (each . with name) of
    Just (x : _) -> pure x
    _ -> _

mkToken = mkWith (\x -> tokens . mAt x)
getToken = getWith (\x -> tokens . mAt x)
mkProof = mkWith (\x -> proofs . mAt x)
getProof = getWith (\x -> proofs . mAt x)
mkAssert = mkWith (\x -> assertions . mAt x)
getAssert = getWith (\x -> assertions . mAt x)
