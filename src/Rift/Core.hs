{- |
 The module for the core definitions used by the solver
-}
module Rift.Core (
  module Base,
  module Dev,
  module Instances,
  module Parser,
  module Unify,
  module Ops,
  module Interface,
) where

import Rift.Core.Base as Base
import Rift.Core.Dev as Dev hiding (FTerm)
import Rift.Core.Dev.Parser as Parser
import Rift.Core.Instances as Instances
import Rift.Core.Interface as Interface
import Rift.Core.Ops as Ops hiding (_frees, _term)
import Rift.Core.Unify as Unify hiding (FTerm)
