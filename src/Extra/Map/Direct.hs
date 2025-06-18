{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DefaultSignatures #-}

module Extra.Map.Direct (Direction (..), Directional (..), DirectMap (..), ltmap, rtmap, lmaprec, rmaprec, normalMap, (->>), (<<-)) where

import Control.Lens qualified as Lens
import Control.Lens.Operators ((%~), (^.), (^?))
import Data.Data
import Extra.Map.Lens
import Extra.Map.Other
import Extra.Map.Types
import GHC.Generics

data Direction
  = LeftToRight
  | RightToLeft
  | Bidirectional
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Enum)

class Directional t where
  direction :: Lens.Lens' t Direction

instance Directional Direction where
  direction = Lens.simple

instance (Directional t) => Directional (t, a) where
  direction = Lens._1 . direction
class (HasMapTag m, (Directional (MapTag m))) => DirectMap m where
  getLeft :: m -> m
  getBoth :: m -> m
  getRight :: m -> m
  getLeftStart :: m -> m
  getRightStart :: m -> m
  default getLeftStart :: (Semigroup m) => m -> m
  default getRightStart :: (Semigroup m) => m -> m
  getLeftStart m = getLeft m <> getBoth m
  getRightStart m = getBoth m <> getRight m
  {-# MINIMAL getLeft, getBoth, getRight #-}

instance (Directional t) => DirectMap (TMap t a b) where
  getLeft (TMap xs) = TMap $ filter (\(TImage t _ _) -> (t ^. direction) == LeftToRight) xs
  getBoth (TMap xs) = TMap $ filter (\(TImage t _ _) -> (t ^. direction) == Bidirectional) xs
  getRight (TMap xs) = TMap $ filter (\(TImage t _ _) -> (t ^. direction) == RightToLeft) xs

ltmap :: (Eq a, Directional t) => TMap t a a -> (a -> a)
ltmap m v =
  case mlookup (getLeft m) v of
    Just x -> x
    Nothing -> v
rtmap :: (Eq a, Directional t) => TMap t a a -> (a -> a)
rtmap m v =
  case mlookupV (getRight m) v of
    Just x -> x
    Nothing -> v
lmaprec' :: (Eq a, Directional t) => TMap t a a -> [a] -> [a] -> (a -> Maybe a)
lmaprec' m u0 u1 v =
  if (v `elem` u0)
    then Nothing
    else case (getLeft m) ^? mAt v . Lens.each . mto of
      Just x -> rmaprec' m (v : u0) u1 x
      Nothing -> Just v

-- TODO: Get this working better
rmaprec' :: (Eq a, Directional t) => TMap t a a -> [a] -> [a] -> (a -> Maybe a)
rmaprec' m u0 u1 v =
  if (v `elem` u1)
    then Nothing
    else case (getRight m) ^? mAtV v . Lens.each . mfrom of
      Just x -> lmaprec' m u0 (v : u1) x
      Nothing -> Just v
lmaprec :: (Eq a, Directional t) => TMap t a a -> (a -> Maybe a)
lmaprec m v = lmaprec' m [] [] v
rmaprec :: (Eq a, Directional t) => TMap t a a -> (a -> Maybe a)
rmaprec m v = rmaprec' m [] [] v

normalMap :: (Directional t, Eq a) => TMap t a a -> Maybe (TMap t a a)
normalMap m =
  (seeMapImg . Lens.each) (normalMap' m) m

normalMap' :: (Directional t, Eq a) => TMap t a a -> TImage t a a -> Maybe (TImage t a a)
normalMap' m (TImage t k v) = case t ^. direction of
  LeftToRight -> TImage t k <$> (rmaprec m v)
  RightToLeft -> flip (TImage t) v <$> (lmaprec m k)
  Bidirectional -> Just $ TImage t k v

(->>) :: a -> b -> TImage Direction a b
(->>) = TImage LeftToRight
(<<-) :: a -> b -> TImage Direction a b
(<<-) = TImage RightToLeft
