{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Basics (Bifunctor (bimap, first, second), split, combine, (<$$>), (</>), (-?>), (|>), (<|), Easy, ListLike, Mapping, iso, xor, (%%), type (->>), implies, traceWith, over, set, view, (%~), (^.), (.~), (<?>), _1, _2) where

import Control.Arrow qualified as Arrow
import Control.Lens (over, set, view, (%~), (&), (.~), (^.), _1, _2)
import Control.Lens qualified as L
import Data.Bifunctor
import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceMarker, traceShow, traceShowId)
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI

-- | The simple boolean implication function, that is \(a \to b\)
implies :: Bool -> Bool -> Bool
implies a b = a || not b

infixr 1 -?>
(-?>) = implies

infixl 4 <$$>

-- | Nested `<$>`
(<$$>) :: (Functor f0) => (Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap

infixl 4 </>
(</>) :: (Monad m) => (a -> m b) -> m a -> m b
(</>) f a = a >>= f

-- | A synoynm for `>>=`
(|>) :: (Monad m) => m a -> (a -> m b) -> m b
(|>) = (>>=)

-- | A synoynm for `=<<`
(<|) :: (Monad m) => (a -> m b) -> m a -> m b
(<|) = flip (|>)

type Mapping a b = [(a, b)]

type Easy a = (Eq a, Show a)
type ListLike box elem = (Functor box, Applicative box, Monad box, Foldable box, Traversable box, Semigroup (box elem), Monoid (box elem))
type ArrayLike box = (Functor box, Applicative box, Monad box, Foldable box)
iso :: (Eq a) => (Eq b) => Mapping a b -> Bool
iso mapping = (all ((\(x0, y0) -> all (\(x1, y1) -> implies (x0 == x1) (y0 == y1)) mapping))) mapping

-- | The boolean exclusive or function
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

(%%) = xor
split :: a -> (a, a)
split a = (a, a)
combine :: (a -> b -> c) -> (a, b) -> c
combine f (a, b) = f a b
infixl 7 %%
type a ->> b = forall arr. (Arrow.Arrow arr) => arr a b

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
