module Lift.Core.Monad.Symbol where

import Control.Lens (At (..), Lens', (%~), (&), (.>), (<.), (?~), (^.), (^?), (^@.), _Just)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS
import Data.Map qualified as Map
import Lift.Core.Forms.Module
import Lift.Core.Monad.Monad
import Lift.Core.Monad.Parse (LiftError (..))
import Lift.Core.Symbol.SymbolTable
import Lift.Core.Symbol.Types

symbolLens :: (Functor f, Applicative f) => QName -> (Maybe Symbol -> f (Maybe Symbol)) -> Universe t1 -> f (Universe t1)
symbolLens (QName modpath symname) = modmap . at modpath . _Just . symboltable . symbols . at symname
getSymbol :: forall t m. (Monad m, MonadFail m, MonadError LiftError m) => QName -> FMT t m Symbol
getSymbol (QName modpath symname) = do
  state <- get
  let res = state ^? universe . symbolLens (QName modpath symname)
  case res of
    Just (Just symbol) -> pure symbol
    _ -> lift $ throwError SymbolNotFound

setSymbol :: forall t m. (Monad m, MonadFail m) => QName -> Symbol -> FMT t m Symbol
setSymbol (QName modpath symname) sym = do
  state <- get
  let res = state & (universe . modmap . at modpath . _Just . symboltable . symbols . at symname) ?~ sym
  put res
  pure sym

mapSymbol :: forall t m. (Monad m, MonadFail m, MonadError LiftError m) => (Symbol -> Symbol) -> QName -> FMT t m Symbol
mapSymbol f (QName modpath symname) = do
  state <- get
  let res = state & (universe . modmap . at modpath . _Just . symboltable . symbols . at symname) %~ fmap f
  put res
  case res ^? universe . modmap . at modpath . _Just . symboltable . symbols . at symname of
    Just (Just symbol) -> pure symbol
    _ -> lift $ throwError SymbolNotFound
