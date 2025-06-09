{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Lift.Common.Tokens where

import Data.Text qualified as T
import Extra
import Lift.Common.Names (Name)

data Arity = Arity
  { _argsBefore :: Int
  , _argsAfter :: Int
  , _results :: Int
  }
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
makeLenses ''Arity

instance Default Arity where
  def = Arity 0 0 1

{- | The type of math tokens
 That is, atomic symbols used in formulas, such as `+`, `x` etc
-}
data MathToken = MathToken
  { _tokenName :: Name
  , _arity :: Arity
  }
  deriving (Eq, Ord, Show, Typeable, Generic, Data)

{- | The type of macros
 Those being shorthand for longer expressions, e.g, `ax_refl` for `∀x (x = x)`
-}
data MacroToken t = MacroToken t
  deriving (Eq, Ord, Show, Typeable, Generic, Data)

makeLenses ''MathToken
makeLenses ''MacroToken
data TokenValue t
  = MathValue MathToken
  | Macro (MacroToken t)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)
makePrisms ''TokenValue
