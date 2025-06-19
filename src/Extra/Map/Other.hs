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

-- | Logical implication: 'implies a b' is true unless 'a' is true and 'b' is false.
implies :: Bool -> Bool -> Bool
implies a b = a || not b

-- | Checks if a list of pairs represents an injective mapping (isomorphism).
iso :: (Eq a, Eq b) => [(a, b)] -> Bool
iso m = all (\(k, v) -> all (\(k', v') -> implies (k == k') (v == v')) m) m

instance Semigroup (TMap t a b) where
  TMap a <> TMap b = TMap (a <> b)

instance Monoid (TMap t a b) where
  mempty = TMap []

-- | Change all occurrences of a value to another value.
change :: (Eq a) => a -> a -> (a -> a)
change from to within = if from == within then to else within

-- | Construct a 'TImage' from a key and value.
image :: a -> b -> TImage () a b
image = TImage ()

-- | Infix synonym for 'image'.
(>->) :: a -> b -> TImage () a b
(>->) = image

-- | Lookup the value for a key in a 'TMap'.
mlookup :: (Eq a) => TMap t a b -> a -> Maybe b
mlookup (TMap []) _ = Nothing
mlookup (TMap ((TImage t k v) : xs)) x = if k == x then Just v else mlookup (TMap xs) x

-- | Lookup the key for a value in a 'TMap'.
mlookupV :: (Eq b) => TMap t a b -> b -> Maybe a
mlookupV (TMap ((TImage t k v) : xs)) y = if v == y then Just k else mlookupV (TMap xs) y
mlookupV (TMap []) _ = Nothing

infixl 2 @>
infixl 2 >@

-- | Apply a mapping to a value, returning the mapped value or the original if not found.
mapSimple :: (Eq a) => HMap a -> a -> a
mapSimple m v =
  case mlookup (m) v of
    Just x -> x
    Nothing -> v

-- | Apply a mapping recursively, following the mapping chain.
mapToF :: (Eq a) => HMap a -> a -> a
mapToF m v = mapToF' m [] v

-- | Apply a mapping recursively in the reverse direction.
mapToFR :: (Eq a) => HMap a -> a -> a
mapToFR m v = mapToFR' m [] v

-- | Helper for 'mapToF', with visited list to avoid cycles.
mapToF' :: (Eq a) => HMap a -> [a] -> a -> a
mapToF' m u v =
  if (v `elem` u)
    then v
    else case mlookup (m) v of
      Just x -> mapToFR' m (v : u) x
      Nothing -> v

-- | Helper for 'mapToFR', with visited list to avoid cycles.
mapToFR' :: (Eq a) => HMap a -> [a] -> a -> a
mapToFR' m u v =
  if (v `elem` u)
    then v
    else case mlookupV (m) v of
      Just x -> mapToF' m (v : u) x
      Nothing -> v

-- | Map all first elements of a bifunctor using a mapping.
(@>) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)

-- | Map all second elements of a bifunctor using a mapping.
(>@) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
mapping >@ value = fmap (first (mapToFR mapping)) value

mapping @> value = fmap (second (mapToF mapping)) value

-- | Check if a mapping is an isomorphism.
miso :: (Eq a) => HMap a -> Bool
miso = iso . fromMap

-- | Check if a mapping is surjective.
msuject :: (Eq a) => (Eq b) => TMap t a b -> Bool
msuject (TMap m) = all (\(TImage t k v) -> all (\(TImage t k' v') -> implies (k == k') (v == v')) m) m

-- | Check if a mapping is injective.
minject :: (Eq a) => (Eq b) => TMap t a b -> Bool
minject (TMap m) = all (\(TImage t k v) -> all (\(TImage t k' v') -> implies (v == v') (k == k')) m) m

-- | Test if a 'TImage' has the given key.
isFrom :: (Eq a, Eq b) => TImage t a b -> a -> Bool
isFrom (TImage t x _) val = x == val

-- | Test if a 'TImage' has the given value.
isTo :: (Eq a, Eq b) => TImage t a b -> b -> Bool
isTo (TImage t _ y) val = y == val

-- | Get all keys from a 'TMap'.
getKeys :: TMap t a b -> [a]
getKeys = foldMap (\(x, _) -> [x]) . fromMap

-- | Get all values from a 'TMap'.
getValues :: TMap t a b -> [b]
getValues = foldMap (\(_, y) -> [y]) . fromMap

-- | Swap keys and values in a 'TMap'.
flipflop :: TMap t a b -> TMap t b a
flipflop (TMap vals) = TMap $ (\(TImage t x y) -> (TImage t y x)) <$> vals

-- | Convert a 'TMap' to a list of pairs.
fromMap :: TMap t a b -> [(a, b)]
fromMap (TMap xs) = map (\(TImage t x y) -> (x, y)) xs

-- | Convert a 'TMap' to a list of 'TImage's.
fromMapImage :: TMap t a b -> [TImage t a b]
fromMapImage (TMap xs) = xs

-- | Convert a list of pairs to a 'TMap'.
toMap :: [(a, b)] -> TMap () a b
toMap xs = TMap $ map (\(x, y) -> TImage () x y) xs

-- | Convert a list of 'TImage's to a 'TMap'.
toMapImage :: [TImage t a b] -> TMap t a b
toMapImage xs = TMap xs

-- | Convert a single pair to a 'TMap'.
toMap1 :: (a, b) -> TMap () a b
toMap1 (x, y) = TMap [TImage () x y]

-- | Recursively apply a mapping until a fixed point is reached.
rmap :: (Eq a) => TMap t a a -> a -> a
rmap m x =
  ( case mlookup m x of
      Just y -> rmap m y
      Nothing -> x
  )

-- | Recursively apply a composed mapping in the key direction.
flipmapK :: (Eq a, Eq b) => TMap t a b -> TMap t a b -> TMap t b a -> a -> a
flipmapK = _

flipmapK_ m0 m1 m2 x =
  ( case mlookup (m0 <> m1) x of
      Just y -> flipmapV m0 m1 m2 y
      Nothing -> x
  )

-- | Recursively apply a composed mapping in the value direction.
flipmapV :: (Eq a, Eq b) => TMap t a b -> TMap t a b -> TMap t b a -> b -> b
flipmapV = _

flipmapV_ m0 m1 m2 x =
  ( case mlookupV (m1 <> m2) x of
      Just y -> flipmapK m0 m1 m2 y
      Nothing -> x
  )
