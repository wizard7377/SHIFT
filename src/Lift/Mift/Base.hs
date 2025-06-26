module Lift.Mift.Base where

import Control.Monad.Identity (Identity)
import Data.Text qualified as T
import Extra
import Lift.Common.Names
import Lift.Common.Parsing (ParseMT)
import Lift.Mift.Expr (MiftExpr)
import Text.Megaparsec (ShowErrorComponent (..))

type MiftM a = ParseMT (ParseInfo MiftExpr) MiftExpr IO a
data ParseInfo t
  = NoSymbol Name
  | AlreadyDefined Name
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

instance (Show t) => ShowErrorComponent (ParseInfo t) where
  showErrorComponent = show
