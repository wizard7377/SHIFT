{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Debug (traceWith, (<?>)) where

import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceMarker, traceShow, traceShowId)
import Extra.Basics as Extra
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI

colorCode :: ANSI.Color -> String
colorCode x = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid x]
resetCode = ANSI.setSGRCode []

-- | Trace a value, and add a snippet of text so that it's easier to track
traceWithStr :: (Show a) => String -> a -> String
traceWithStr msg a = (colorCode ANSI.Red ++ msg ++ ": " ++ colorCode ANSI.Yellow ++ show a ++ resetCode)

traceWith msg a = trace (traceWithStr msg a) a
(<?>) :: (Show a) => String -> a -> a
msg <?> val = traceWith msg val

infixr 0 <?>
traceWithStack :: (HasCallStack) => (Show a) => T.Text -> a -> a
traceWithStack msg a = trace (Stack.prettyCallStack Stack.callStack <> T.unpack msg <> ": " <> show a) a
