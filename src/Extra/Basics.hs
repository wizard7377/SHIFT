module Extra.Basics where

import Control.Arrow qualified as Arrow
import Data.Text qualified as T
import Debug.Trace (trace, traceShow, traceShowId)

-- | The simple boolean implication function, that is \(a \to b\)
implies :: Bool -> Bool -> Bool
implies a b = a || not b

-- | Nested `<$>`
(<$$>) :: (Functor f0) => (Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap

-- | A synoynm for `>>=`
(|>) :: (Monad m) => m a -> (a -> m b) -> m b
(|>) = (>>=)

-- | A synoynm for `=<<`
(<|) :: (Monad m) => (a -> m b) -> m a -> m b
(<|) = flip (|>)

type Mapping a b = [(a, b)]

iso :: (Eq a) => (Eq b) => Mapping a b -> Bool
iso mapping = (all ((\(x0, y0) -> all (\(x1, y1) -> implies (x0 == x1) (y0 == y1)) mapping))) mapping

-- | The boolean exclusive or function
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

(%%) = xor
infixl 7 %%
type a ->> b = forall arr. (Arrow.Arrow arr) => arr a b

-- | Trace a value, and add a snippet of text so that it's easier to track
traceWith :: (Show a) => T.Text -> a -> a
traceWith msg a = trace (T.unpack msg <> ": " <> show a) a
