{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Debug () where

import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceMarker, traceShow, traceShowId)
import Extra.Basics as Extra
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI
