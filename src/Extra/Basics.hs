{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Extra.Basics (
  Known,
  Bifunctor (bimap, first, second),
  split,
  combine,
  Easy,
  ListLike,
  Mapping,
  over,
  set,
  view,
  _1,
  _2,
  amap,
  Type,
  Constraint,
  Data,
  Generic,
) where

import Control.Arrow qualified as Arrow
import Control.Lens (over, set, view, (%~), (&), (.~), (^.), _1, _2)
import Control.Lens qualified as L
import Data.Bifunctor
import Data.Data
import Data.Kind (Constraint, Type)
import Data.Text qualified as T
import Debug.Trace (trace, traceEvent, traceMarker, traceShow, traceShowId)
import GHC.Generics (Generic)
import GHC.IO (unsafePerformIO)
import GHC.Stack (HasCallStack)
import GHC.Stack qualified as Stack
import Language.Haskell.TH qualified as TH
import System.Console.ANSI qualified as ANSI

type Known a = (Typeable a, Data a, Show a, Eq a)

type Mapping a b = [(a, b)]

type Easy a = (Eq a, Show a)
type ListLike box elem = (Functor box, Applicative box, Monad box, Foldable box, Traversable box, Semigroup (box elem), Monoid (box elem))
type ArrayLike box = (Functor box, Applicative box, Monad box, Foldable box)

amap :: (Applicative f) => f (a -> b) -> f a -> f b
amap = (<*>)
split :: a -> (a, a)
split a = (a, a)
combine :: (a -> b -> c) -> (a, b) -> c
combine f (a, b) = f a b
