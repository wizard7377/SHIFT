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
import Data.Maybe (catMaybes, fromMaybe)
import Extra
import Rift.Core.Base
import Prelude hiding ((.))

-- | The state of a term, where it being bound always features the variable first
data TermState a
  = Free a
  | Bound a a
  deriving (Show, Eq, Data, Typeable, Generic, Ord)

makePrisms ''TermState
data UnifyState term = UnifyState
  { _upState :: [TermState term]
  , _downState :: [TermState term]
  , _renames :: HMap term
  }
  deriving (Ord, Data, Typeable, Generic)

deriving instance (Show term) => Show (UnifyState term)
deriving instance (Eq term) => Eq (UnifyState term)

makeLenses ''UnifyState

replaceDown :: (TermLike term) => term -> term -> UnifyState term -> UnifyState term
replaceDown old new state =
  let
    r0 = state & upState . each . _Bound . _2 %~ (change old new)
    r1 = r0 & downState . each . _Free %~ (change old new)
    r2 = r1 & downState . each . _Bound . _2 %~ (change old new)
    r3 = r2 & renames %~ (<> (toMapImage $ pure $ Image old new))
   in
    r3
mapUp :: (TermLike term) => UnifyState term -> term -> term
mapUp state x = fromMaybe x (mapUp' [] state x)
mapUp' :: (TermLike term) => [Either term term] -> UnifyState term -> term -> Maybe term
mapUp' used state x =
  ("Used" <?> used) `seq`
    ("State" <?> state) `seq`
      ("x" <?> x) `seq`
        "Result"
          <?> if (Left x `elem` used)
            then Nothing
            else case getAt (state ^. upState) x of
              Just (Free y) -> Just y
              Just (Bound _ y) -> mapDown' (Left x : used) state y
              _ -> Just x

mapDown :: (TermLike term) => UnifyState term -> term -> term
mapDown state x = fromMaybe x (mapDown' [] state x)
mapDown' :: (TermLike term) => [Either term term] -> UnifyState term -> term -> Maybe term
mapDown' used state y =
  if (Right y `elem` used)
    then Nothing
    else case (mlookup (state ^. renames) y, getAt (state ^. downState) y) of
      (Just z, _) -> mapDown' (Right y : used) state z
      (_, Just (Free x)) -> Just x
      (_, Just (Bound x _)) -> mapUp' (Right y : used) state x
      (_, Nothing) -> Just y
isFree :: (Eq term) => term -> [TermState term] -> Bool
isFree _ [] = False
isFree x (Free y : xs) =
  if x == y
    then True
    else isFree x xs
isFree x (Bound y _ : xs) = isFree x xs
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

setAtB :: (Eq term) => term -> TermState term -> [TermState term] -> [TermState term]
setAtB _ _ [] = []
setAtB y z (Free x : xs) =
  if x == y
    then z : xs
    else Free x : setAt y z xs
setAtB z w (Bound x y : xs) =
  if y == z
    then w : xs
    else Bound x y : setAt z w xs

instance (Eq term) => Semigroup (UnifyState term) where
  UnifyState a c e <> UnifyState d f h =
    UnifyState (a <> d) (c <> f) (e <> h)
instance (Eq term) => Monoid (UnifyState term) where
  mempty = UnifyState [] [] (toMap [])

type UnifyState' t = Choice (UnifyState t)
type Unification t = (TermLike t) => (Term t) => t -> t -> UnifyState t -> UnifyState' t
getBound :: [TermState t] -> [t]
getBound l = catMaybes $ map (\case Bound x _ -> Just x; _ -> Nothing) l
getFree l = catMaybes $ map (\case Free x -> Just x; _ -> Nothing) l
