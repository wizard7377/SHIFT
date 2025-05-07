{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Debug (traceWith, (<?>), (<?@>), (?@>), (?@>>), (>?>), (>?@>), showColor, showColor', getColor, makeColor, resetCode, (?>>), (?>), traceWithStr) where

import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceEventWith, traceM, traceMarker, traceShow, traceShowId)
import Extra.Basics as Extra
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI

colorList = ANSI.Red : ANSI.Blue : ANSI.Green : []
getColor :: Int -> ANSI.Color
getColor v = colorList !! mod v 3
colorCode :: ANSI.Color -> String
colorCode x = ANSI.setSGRCode [ANSI.SetColor ANSI.Foreground ANSI.Vivid x]
resetCode = ANSI.setSGRCode []

makeColor color str = colorCode color ++ str ++ resetCode
showColor :: (Show a) => Int -> a -> String
showColor i a = colorCode (getColor i) ++ show a ++ resetCode
showColor' :: Int -> String -> String
showColor' i a = colorCode (getColor i) ++ a ++ resetCode

-- | Trace a value, and add a snippet of text so that it's easier to track
traceWithStr :: (Show a) => String -> a -> String
traceWithStr msg a = (colorCode ANSI.Red ++ msg ++ ": " ++ colorCode ANSI.Yellow ++ show a ++ resetCode)

traceWith msg a = traceEvent (traceWithStr msg a) a
traceWithM :: (Applicative f) => (Show a) => String -> a -> f a
traceWithM msg a = traceM (traceWithStr msg a) *> pure a
(<?>) :: (Show a) => String -> a -> a
msg <?> val = traceWith msg val
msg <?@> val = val
(?>) :: String -> a -> a
(>?>) :: (Show a) => String -> a -> (b -> b)
msg >?> val = \x -> seq (traceEvent msg val) x
msg >?@> val = id
(?@>) :: String -> a -> a
msg ?> val = traceEvent msg val
msg ?@> val = val
(?>>) :: (Applicative f) => (Show a) => String -> a -> f a
msg ?>> val = traceWithM msg val
msg ?@>> val = pure val
infixr 0 <?>
infixr 0 <?@>
infixr 0 ?>>
infixr 0 ?@>>
infixr 0 ?>
infixr 0 ?@>
infixr 0 >?>
infixr 0 >?@>
traceWithStack :: (HasCallStack) => (Show a) => T.Text -> a -> a
traceWithStack msg a = trace (Stack.prettyCallStack Stack.callStack <> T.unpack msg <> ": " <> show a) a
