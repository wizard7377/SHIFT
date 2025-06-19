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

-- | Direction of a mapping: left-to-right, right-to-left, or bidirectional.
data Direction
  = LeftToRight
  | RightToLeft
  | Bidirectional
  deriving (Eq, Ord, Show, Data, Typeable, Generic, Enum)

-- | Typeclass for types that have a 'Direction'.
class Directional t where
  -- | Lens to access the 'Direction' of a value.
  direction :: Lens.Lens' t Direction

instance Directional Direction where
  direction = Lens.simple

instance (Directional t) => Directional (t, a) where
  direction = Lens._1 . direction

-- | Typeclass for maps that support directional operations.
class (HasMapTag m, (Directional (MapTag m))) => DirectMap m where
  -- | Get only left-to-right mappings.
  getLeft :: m -> m

  -- | Get only bidirectional mappings.
  getBoth :: m -> m

  -- | Get only right-to-left mappings.
  getRight :: m -> m

  -- | Get left-to-right and bidirectional mappings.
  getLeftStart :: m -> m

  -- | Get right-to-left and bidirectional mappings.
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

-- | Apply only left-to-right mappings.
ltmap :: (Eq a, Directional t) => TMap t a a -> (a -> a)
ltmap m v =
  case mlookup (getLeft m) v of
    Just x -> x
    Nothing -> v

-- | Apply only right-to-left mappings.
rtmap :: (Eq a, Directional t) => TMap t a a -> (a -> a)
rtmap m v =
  case mlookupV (getRight m) v of
    Just x -> x
    Nothing -> v

-- | Recursively apply left-to-right mappings, avoiding cycles.
lmaprec' :: (Eq a, Directional t) => TMap t a a -> [a] -> [a] -> (a -> Maybe a)
lmaprec' m u0 u1 v =
  if (v `elem` u0)
    then Nothing
    else case (getLeft m) ^? mAt v . Lens.each . mto of
      Just x -> rmaprec' m (v : u0) u1 x
      Nothing -> Just v

-- | Recursively apply right-to-left mappings, avoiding cycles.
rmaprec' :: (Eq a, Directional t) => TMap t a a -> [a] -> [a] -> (a -> Maybe a)
rmaprec' m u0 u1 v =
  if (v `elem` u1)
    then Nothing
    else case (getRight m) ^? mAtV v . Lens.each . mfrom of
      Just x -> lmaprec' m u0 (v : u1) x
      Nothing -> Just v

-- | Recursively apply left-to-right mappings.
lmaprec :: (Eq a, Directional t) => TMap t a a -> (a -> Maybe a)
lmaprec m v = lmaprec' m [] [] v

-- | Recursively apply right-to-left mappings.
rmaprec :: (Eq a, Directional t) => TMap t a a -> (a -> Maybe a)
rmaprec m v = rmaprec' m [] [] v

-- | Normalize a mapping by recursively resolving all mappings.
normalMap :: (Directional t, Eq a) => TMap t a a -> Maybe (TMap t a a)
normalMap m =
  (seeMapImg . Lens.each) (normalMap' m) m

-- | Helper for 'normalMap', normalizes a single image.
normalMap' :: (Directional t, Eq a) => TMap t a a -> TImage t a a -> Maybe (TImage t a a)
normalMap' m (TImage t k v) = case t ^. direction of
  LeftToRight -> TImage t k <$> (rmaprec m v)
  RightToLeft -> flip (TImage t) v <$> (lmaprec m k)
  Bidirectional -> Just $ TImage t k v

-- | Infix constructor for left-to-right 'TImage'.
(->>) :: a -> b -> TImage Direction a b
(->>) = TImage LeftToRight

-- | Infix constructor for right-to-left 'TImage'.
(<<-) :: a -> b -> TImage Direction a b
(<<-) = TImage RightToLeft

instance {-# OVERLAPPING #-} (Show a, Show b) => Show (TImage Direction a b) where
  show (TImage LeftToRight x y) = show x ++ " >>->> " ++ show y
  show (TImage RightToLeft x y) = show x ++ " <<-<< " ++ show y
  show (TImage Bidirectional x y) = show x ++ " <<->> " ++ show y
