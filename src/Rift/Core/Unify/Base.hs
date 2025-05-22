{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Unify.Base where

import Control.Applicative
import Control.Category (Category (..))
import Control.Lens (Lens', makeLenses)
import Control.Lens.Indexed (indexing)
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.List
import Data.Maybe (catMaybes)
import Extra
import Rift.Core.Base

-- | The state of a term, where it being bound always features the variable first
data TermState a
  = Free a
  | Bound a a
  deriving (Show, Eq, Data, Typeable, Generic, Ord)

data UnifyState term = UnifyState
  { _upState :: [TermState term]
  , _downState :: [TermState term]
  }
  deriving (Ord, Data, Typeable, Generic)

deriving instance (Show term) => Show (UnifyState term)
deriving instance (Eq term) => Eq (UnifyState term)

makeLenses ''UnifyState

mapUp :: (TermLike term) => UnifyState term -> term -> term
mapUp = mapUp' []
mapUp' used state x =
  if (Left x `elem` used)
    then x
    else case getAt (state ^. upState) x of
      Just (Free y) -> y
      Just (Bound _ y) -> mapDown' (Left x : used) state y
      Nothing -> x

mapDown :: (TermLike term) => UnifyState term -> term -> term
mapDown = mapDown' []
mapDown' used state y =
  if (Right y `elem` used)
    then y
    else case getAt (state ^. downState) y of
      Just (Free x) -> x
      Just (Bound x _) -> mapUp' (Right y : used) state x
      Nothing -> y
isFree :: (Eq term) => term -> [TermState term] -> Bool
isFree _ [] = False
isFree x (Free y : xs) =
  if x == y
    then True
    else isFree x xs
getBinds :: (Eq term) => term -> [TermState term] -> [term]
getBinds x term = catMaybes $ fmap (\case Bound x' y -> if x == x' then Just y else Nothing; _ -> Nothing) term
setBind :: (Eq term) => term -> term -> [TermState term] -> ([TermState term], Bool)
setBind _ _ [] = ([], False)
setBind x y (Free z : xs) =
  if x == z
    then (((Bound x y) : xs), True)
    else setBind x y xs
getAt :: (Eq term) => [TermState term] -> term -> Maybe (TermState term)
getAt [] _ = Nothing
getAt (Free x : xs) y =
  if x == y
    then Just (Free x)
    else getAt xs y
getAt (Bound x y : xs) z =
  if x == z
    then Just (Bound x y)
    else getAt xs z

setAt :: (Eq term) => term -> TermState term -> [TermState term] -> [TermState term]
setAt _ _ [] = []
setAt y z (Free x : xs) =
  if x == y
    then z : xs
    else Free x : setAt y z xs
setAt z w (Bound x y : xs) =
  if x == z
    then w : xs
    else Bound x y : setAt z w xs

instance (Eq term) => Semigroup (UnifyState term) where
  UnifyState a c <> UnifyState d f =
    UnifyState (a <> d) (c <> f)
instance (Eq term) => Monoid (UnifyState term) where
  mempty = UnifyState [] []

type UnifyState' t = Choice (UnifyState t)
type Unification t = (TermLike t) => (Term t) => t -> t -> UnifyState t -> UnifyState' t
