{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Debug (traceWith, (<?>), (<?@>), (?@>), (?@>>), (>?>), (>?@>), (?>>), (?>), traceWithStr) where

import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceEventWith, traceM, traceMarker, traceShow, traceShowId)
import Extra.Basics as Extra
import Extra.Color (colorCode, makeColor, resetCode, showColor, showColor')
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI

{-# INLINE traceF #-}
traceF :: String -> a -> a
#ifdef debug
traceF !msg val = trace msg val
#else 
traceF = const id
#endif
{-# INLINE traceWithStr #-}

-- | Trace a value, and add a snippet of text so that it's easier to track
traceWithStr :: (Show a) => String -> a -> String
traceWithStr msg a = (colorCode ANSI.Red ++ msg ++ ": " ++ colorCode ANSI.Yellow ++ show a ++ resetCode)

{-# INLINE traceWith #-}
traceWith msg a = traceF (traceWithStr msg a) a
traceWithM :: (Applicative f) => (Show a) => String -> a -> f a
traceWithM msg a = traceM (traceWithStr msg a) *> pure a
{-# INLINE (<?>) #-}
(<?>) :: (Show a) => String -> a -> a
msg <?> val = traceWith msg val
{-# INLINE (<?@>) #-}
_ <?@> val = val
{-# INLINE (?>) #-}
(?>) :: String -> a -> a
{-# INLINE (>?>) #-}
(>?>) :: (Show a) => String -> a -> (b -> b)
msg >?> val = \x -> seq (traceF msg val) x
{-# INLINE (>?@>) #-}
_ >?@> val = id
(?@>) :: String -> a -> a
msg ?> val = seq (traceF msg) val
_ ?@> val = val
(?>>) :: (Applicative f) => (Show a) => String -> a -> f a
msg ?>> val = traceWithM msg val
_ ?@>> val = pure val
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
