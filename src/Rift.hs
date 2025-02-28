module Rift (
  module Rift.Base,
  module Rift.Parser,
  module Rift.Unify,
  module Rift.Dev,
  (>@>),
  (>?>),
  Term (..),
  Unification,
  unify,
) where

-- \|
--    The representation for Flat Types
import Rift.Base
import Rift.Dev
import Rift.Instances ()
import Rift.Parser
import Rift.Unify

-- ^ Flat Intermediate Representation
