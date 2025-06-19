{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Extra.Map.Lens (
  mAt,
  mAtV,
  mAtT,
  mto,
  mfrom,
  getFrom,
  getTo,
  seeMapImg,
  seeImgTup,
  seeMapTup,
  seeImgMap,
  seeTupImg,
  seeTupMap,
) where

import Control.Lens (Lens', lens)
import Control.Lens qualified as Lens
import Data.Tuple.Extra (uncurry3)
import Extra.Map.Other (fromMap, fromMapImage, image, mlookup, toMap, toMapImage)
import Extra.Map.Types
import Extra.Ops

-- | Helper to uncurry a function on the second argument.
uncurrysnd :: (a -> b -> c -> d) -> (a -> ((b, c) -> d))
uncurrysnd f a (b, c) = f a b c

-- | Lens for the source (from) of a 'TImage'.
mfrom :: Lens' (TImage t a b) a
mfrom = Lens.lens (\(TImage _ x _) -> x) (\(TImage t _ y) x -> TImage t x y)

-- | Lens for the target (to) of a 'TImage'.
mto :: Lens' (TImage t a b) b
mto = Lens.lens (\(TImage _ _ y) -> y) (\(TImage t x _) y -> TImage t x y)

-- | Lens for the tag of a 'TImage'.
mtag :: Lens' (TImage t a b) t
mtag = Lens.lens (\(TImage t _ _) -> t) (\(TImage _ x y) t -> TImage t x y)

-- | Get the first 'TImage' with the given key from a 'TMap'.
getFrom :: (Eq a) => a -> TMap t a b -> Maybe (TImage t a b)
getFrom k (TMap ((i@(TImage t x y)) : m)) = if x == k then Just i else getFrom k (TMap m)
getFrom _ _ = Nothing

-- | Get the first 'TImage' with the given value from a 'TMap'.
getTo :: (Eq b) => b -> TMap t a b -> Maybe (TImage t a b)
getTo v (TMap ((i@(TImage t x y)) : m)) = if y == v then Just i else getTo v (TMap m)
getTo _ _ = Nothing

-- | Add a 'TImage' to the head of a 'TMap'.
mAddHead :: TImage t a b -> (TMap t a b) -> TMap t a b
mAddHead i (TMap m) = TMap (i : m)

-- | Map over a key in a 'TMap', with a default value if not found.
mapFrom :: (Eq a) => a -> Maybe (TImage t a b) -> (TImage t a b -> TImage t a b) -> (TMap t a b) -> (TMap t a b)
mapFrom k i' f (TMap ((i@(TImage t x y)) : m)) = if k == x then TMap ((f i) : m) else mAddHead i (mapFrom k i' f (TMap m))
mapFrom k (Just i') n (TMap []) = TMap (pure i')
mapFrom k Nothing n (TMap []) = TMap []

-- | Map over a value in a 'TMap', with a default value if not found.
mapTo :: (Eq b) => b -> Maybe (TImage t a b) -> (TImage t a b -> TImage t a b) -> (TMap t a b) -> (TMap t a b)
mapTo v i' f (TMap ((i@(TImage t x y)) : m)) = if v == y then TMap ((f i) : m) else mAddHead i (mapTo v i' f (TMap m))
mapTo v (Just i') f (TMap []) = TMap (pure i')
mapTo v (Nothing) f (TMap []) = TMap []

type instance Lens.Index (TMap t a b) = a
type instance Lens.IxValue (TMap t a b) = b

-- | Get all 'TImage's with the given tag.
mAtGet1 :: (Eq t) => t -> (TMap t k v -> [TImage t k v])
mAtGet1 t (fromTMap -> m) = (uncurry3 TImage) <$> (filter ((\x -> (x ^. Lens._1) == t)) m)

-- | Set all 'TImage's with the given tag.
mAtSet1 :: (Eq t) => t -> (TMap t k v -> [TImage t k v] -> TMap t k v)
mAtSet1 t (fromTMap -> m) l =
  let newImages = (filter (not . (\x -> (x ^. Lens._1) == t)) m)
   in (toTMap newImages) <> (l ^. (Lens.re seeMapImg))

-- | Get all 'TImage's with the given key.
mAtGet2 :: (Eq k) => k -> (TMap t k v -> [TImage t k v])
mAtGet2 k (fromTMap -> m) = (uncurry3 TImage) <$> (filter ((\x -> (x ^. Lens._2) == k)) m)

-- | Set all 'TImage's with the given key.
mAtSet2 :: (Eq k) => k -> (TMap t k v -> [TImage t k v] -> TMap t k v)
mAtSet2 k (fromTMap -> m) l =
  let newImages = (filter (not . (\x -> (x ^. Lens._2) == k)) m)
   in (toTMap newImages) <> (l ^. (Lens.re seeMapImg))

-- | Get all 'TImage's with the given value.
mAtGet3 :: (Eq v) => v -> (TMap t k v -> [TImage t k v])
mAtGet3 v (fromTMap -> m) = (uncurry3 TImage) <$> (filter ((\x -> (x ^. Lens._3) == v)) m)

-- | Set all 'TImage's with the given value.
mAtSet3 :: (Eq v) => v -> (TMap t k v -> [TImage t k v] -> TMap t k v)
mAtSet3 v (fromTMap -> m) l =
  let newImages = (filter (not . (\x -> (x ^. Lens._3) == v)) m)
   in (toTMap newImages) <> (l ^. (Lens.re seeMapImg))

-- | Lens at key.
mAt :: (Eq k) => k -> Lens.Lens' (TMap t k v) [TImage t k v]
mAt k = lens (mAtGet2 k) (mAtSet2 k)

-- | Lens at value.
mAtV :: (Eq v) => v -> Lens.Lens' (TMap t k v) [TImage t k v]
mAtV v = lens (mAtGet3 v) (mAtSet3 v)

-- | Lens at tag.
mAtT :: (Eq t) => t -> Lens.Lens' (TMap t k v) [TImage t k v]
mAtT t = lens (mAtGet1 t) (mAtSet1 t)

-- | Iso between 'TMap' and list of 'TImage's.
seeMapImg :: Lens.Iso' (TMap t k v) [TImage t k v]
seeMapImg = Lens.iso fromMapImage (\i -> toMapImage i)

-- | Iso between 'TImage' and tuple.
seeImgTup :: Lens.Iso' (TImage t k v) (t, k, v)
seeImgTup = Lens.iso (\(TImage t k v) -> (t, k, v)) (\(t, k, v) -> (TImage t k v))

-- | Iso between 'TMap' and list of tuples.
seeMapTup :: Lens.Iso' (TMap t a b) [(t, a, b)]
seeMapTup = Lens.iso fromTMap toTMap

-- | Iso between tuple and 'TImage'.
seeTupImg :: Lens.Iso' (t, k, v) (TImage t k v)
seeTupImg = Lens.from seeImgTup

-- | Iso between list of 'TImage's and 'TMap'.
seeImgMap :: Lens.Iso' [TImage t k v] (TMap t k v)
seeImgMap = Lens.from seeMapImg

-- | Iso between list of tuples and 'TMap'.
seeTupMap :: Lens.Iso' [(t, a, b)] (TMap t a b)
seeTupMap = Lens.from seeMapTup

-- | Convert a 'TMap' to a list of tuples.
fromTMap :: TMap t a b -> [(t, a, b)]
fromTMap (TMap xs) = (\(TImage t a b) -> (t, a, b)) <$> xs

-- | Convert a list of tuples to a 'TMap'.
toTMap :: [(t, a, b)] -> TMap t a b
toTMap xs = TMap (uncurry3 TImage <$> xs)
