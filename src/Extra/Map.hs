{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.Map where

import Control.Category qualified as Category
import Control.Lens qualified as Lens
import Data.Bifunctor (Bifunctor (..))
import Data.Data (Data)
import Data.Maybe (fromJust, fromMaybe)
import Data.Typeable
import GHC.Generics (Generic)

implies :: Bool -> Bool -> Bool
implies a b = a || not b
iso :: (Eq a, Eq b) => [(a, b)] -> Bool
iso m = all (\(k, v) -> all (\(k', v') -> implies (k == k') (v == v')) m) m

-- | A projection of a value of type @a@ onto a value of type @b@
data Image a b = Image a b
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

type HImage a = Image a a

-- | A colelction of projections forming a mapping from @a@ to @b@
data Map a b = Map [Image a b]
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- | A mapping from @a@ to itself
type HMap a = Map a a

change :: (Eq a) => a -> a -> (a -> a)
change from to within = if from == within then to else within
image :: a -> b -> Image a b
image = Image
(>->) :: a -> b -> Image a b
(>->) = image
mlookup :: (Eq a) => Map a b -> a -> Maybe b
mlookup (Map []) _ = Nothing
mlookup (Map ((Image k v) : xs)) x = if k == x then Just v else mlookup (Map xs) x
mlookupV :: (Eq b) => Map a b -> b -> Maybe a
mlookupV (Map ((Image k v) : xs)) y = if v == y then Just k else mlookupV (Map xs) y
mlookupV (Map []) _ = Nothing
infixl 2 @>
infixl 2 >@
mapSimple :: (Eq a) => HMap a -> a -> a
mapSimple m v =
  case mlookup m v of
    Just x -> x
    Nothing -> v
mapToF :: (Eq a) => HMap a -> a -> a
mapToF m v = mapToF' m [] v
mapToFR :: (Eq a) => HMap a -> a -> a
mapToFR m v = mapToFR' m [] v

-- | Map to function, first as input, second as output
mapToF' :: (Eq a) => HMap a -> [a] -> a -> a
mapToF' m u v =
  if (v `elem` u)
    then v
    else case mlookup m v of
      Just x -> mapToFR' m (v : u) x
      Nothing -> v

-- | Map to function, second as input, first as output
mapToFR' :: (Eq a) => HMap a -> [a] -> a -> a
mapToFR' m u v =
  if (v `elem` u)
    then v
    else case mlookupV m v of
      Just x -> mapToF' m (v : u) x
      Nothing -> v

(@>) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
(>@) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
mapping >@ value = fmap (first (mapToFR mapping)) value
mapping @> value = fmap (second (mapToF mapping)) value
miso :: (Eq a) => HMap a -> Bool
miso = iso . fromMap

msuject :: (Eq a) => (Eq b) => Map a b -> Bool
msuject (Map m) = all (\(Image k v) -> all (\(Image k' v') -> implies (k == k') (v == v')) m) m
minject :: (Eq a) => (Eq b) => Map a b -> Bool
minject (Map m) = all (\(Image k v) -> all (\(Image k' v') -> implies (v == v') (k == k')) m) m

isFrom :: (Eq a, Eq b) => Image a b -> a -> Bool
isFrom (Image x _) val = x == val
isTo :: (Eq a, Eq b) => Image a b -> b -> Bool
isTo (Image _ y) val = y == val

getKeys :: Map a b -> [a]
getKeys = foldMap (\(x, _) -> [x]) . fromMap

getValues :: Map a b -> [b]
getValues = foldMap (\(_, y) -> [y]) . fromMap
flipflop :: Map a b -> Map b a
flipflop (Map vals) = Map $ (\(Image x y) -> (Image y x)) <$> vals

fromMap :: Map a b -> [(a, b)]
fromMap (Map xs) = map (\(Image x y) -> (x, y)) xs
fromMapImage :: Map a b -> [Image a b]
fromMapImage (Map xs) = xs
toMap :: [(a, b)] -> Map a b
toMap xs = Map $ map (\(x, y) -> Image x y) xs
toMapImage :: [Image a b] -> Map a b
toMapImage xs = Map xs

instance (Eq a) => Semigroup (HMap a) where
  Map a <> Map b = Map (a <> b)
