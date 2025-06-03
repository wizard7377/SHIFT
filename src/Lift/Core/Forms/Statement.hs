module Lift.Core.Forms.Statement where

import Data.Text qualified as T
import Extra

data DefinitionType
  = Axiom
  | Theorem
  | Definition
  | Lemma
  | Fact
  deriving (Eq, Show, Ord, Data, Generic)

data ConvertMethod
  = Exact
  | AlphaTop
  deriving (Eq, Show, Ord, Data, Generic)

data Proof t = Proof
  { _goal :: t
  , _hypo :: t
  , _method :: [ConvertMethod]
  }
  deriving (Eq, Show, Ord, Data, Generic)
