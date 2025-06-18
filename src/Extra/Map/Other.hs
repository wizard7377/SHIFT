module Extra.Map.Other where

import Control.Category qualified as Category
import Control.Lens qualified as Lens
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import Data.Typeable
import Extra.Lens
import Extra.Map.Types
import GHC.Generics (Generic)

implies :: Bool -> Bool -> Bool
implies a b = a || not b
iso :: (Eq a, Eq b) => [(a, b)] -> Bool
iso m = all (\(k, v) -> all (\(k', v') -> implies (k == k') (v == v')) m) m

instance Semigroup (TMap t a b) where
  TMap a <> TMap b = TMap (a <> b)
instance Monoid (TMap t a b) where
  mempty = TMap []

change :: (Eq a) => a -> a -> (a -> a)
change from to within = if from == within then to else within
image :: a -> b -> TImage () a b
image = TImage ()
(>->) :: a -> b -> TImage () a b
(>->) = image

mlookup :: (Eq a) => TMap t a b -> a -> Maybe b
mlookup (TMap []) _ = Nothing
mlookup (TMap ((TImage t k v) : xs)) x = if k == x then Just v else mlookup (TMap xs) x
mlookupV :: (Eq b) => TMap t a b -> b -> Maybe a
mlookupV (TMap ((TImage t k v) : xs)) y = if v == y then Just k else mlookupV (TMap xs) y
mlookupV (TMap []) _ = Nothing
infixl 2 @>
infixl 2 >@
mapSimple :: (Eq a) => HMap a -> a -> a
mapSimple m v =
  case mlookup (m) v of
    Just x -> x
    Nothing -> v
mapToF :: (Eq a) => HMap a -> a -> a
mapToF m v = mapToF' m [] v
mapToFR :: (Eq a) => HMap a -> a -> a
mapToFR m v = mapToFR' m [] v

-- | TMap to function, first as input, second as output
mapToF' :: (Eq a) => HMap a -> [a] -> a -> a
mapToF' m u v =
  if (v `elem` u)
    then v
    else case mlookup (m) v of
      Just x -> mapToFR' m (v : u) x
      Nothing -> v

-- | TMap to function, second as input, first as output
mapToFR' :: (Eq a) => HMap a -> [a] -> a -> a
mapToFR' m u v =
  if (v `elem` u)
    then v
    else case mlookupV (m) v of
      Just x -> mapToF' m (v : u) x
      Nothing -> v

(@>) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
(>@) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
mapping >@ value = fmap (first (mapToFR mapping)) value
mapping @> value = fmap (second (mapToF mapping)) value
miso :: (Eq a) => HMap a -> Bool
miso = iso . fromMap

msuject :: (Eq a) => (Eq b) => TMap t a b -> Bool
msuject (TMap m) = all (\(TImage t k v) -> all (\(TImage t k' v') -> implies (k == k') (v == v')) m) m
minject :: (Eq a) => (Eq b) => TMap t a b -> Bool
minject (TMap m) = all (\(TImage t k v) -> all (\(TImage t k' v') -> implies (v == v') (k == k')) m) m

isFrom :: (Eq a, Eq b) => TImage t a b -> a -> Bool
isFrom (TImage t x _) val = x == val
isTo :: (Eq a, Eq b) => TImage t a b -> b -> Bool
isTo (TImage t _ y) val = y == val

getKeys :: TMap t a b -> [a]
getKeys = foldMap (\(x, _) -> [x]) . fromMap

getValues :: TMap t a b -> [b]
getValues = foldMap (\(_, y) -> [y]) . fromMap
flipflop :: TMap t a b -> TMap t b a
flipflop (TMap vals) = TMap $ (\(TImage t x y) -> (TImage t y x)) <$> vals

fromMap :: TMap t a b -> [(a, b)]
fromMap (TMap xs) = map (\(TImage t x y) -> (x, y)) xs
fromMapImage :: TMap t a b -> [TImage t a b]
fromMapImage (TMap xs) = xs
toMap :: [(a, b)] -> TMap () a b
toMap xs = TMap $ map (\(x, y) -> TImage () x y) xs
toMapImage :: [TImage t a b] -> TMap t a b
toMapImage xs = TMap xs
toMap1 :: (a, b) -> TMap () a b
toMap1 (x, y) = TMap [TImage () x y]
rmap m x =
  ( case mlookup m x of
      Just y -> rmap m y
      Nothing -> x
  )
flipmapK m0 m1 m2 x =
  ( case mlookup (m0 <> m1) x of
      Just y -> flipmapV m0 m1 m2 y
      Nothing -> x
  )
flipmapV m0 m1 m2 x =
  ( case mlookupV (m1 <> m2) x of
      Just y -> flipmapK m0 m1 m2 y
      Nothing -> x
  )
