module Sift.Core.Dev.Paeno where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Extra
import Rift.Core.Base
import Sift.Core.Dev.Parser
import Sift.Core.Dev.Util
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseTest)

-- sysBase = readSys "test/Simple.tift"
-- sys1 = selectAt sysBase [0]
-- sys2 = selectAt sysBase [0 .. 1]
-- sys3 = selectAt sysBase [0 .. 2]
