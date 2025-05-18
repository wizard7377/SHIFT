module Rift.Core.Dev.Paeno where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Extra
import Rift.Core.Base
import Rift.Core.Dev.Parser
import Rift.Core.Dev.Util
import Rift.Core.Unify.Base
import Rift.Core.Unify.Unify
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseTest)

sysBase = readSys "test/Simple.tift"
sys1 = selectAt sysBase [0]
sys2 = selectAt sysBase [0 .. 1]
sys3 = selectAt sysBase [0 .. 2]
