{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Extra.Map.Lens where

import Control.Lens (Lens', lens)
import Control.Lens qualified as Lens
import Extra.Map.Other (fromMap, fromMapImage, image, mlookup, toMap, toMapImage)
import Extra.Map.Types

mfrom :: Lens' (Image a b) a
mfrom = Lens.lens (\(Image x _) -> x) (\(Image _ y) x -> Image x y)
mto :: Lens' (Image a b) b
mto = Lens.lens (\(Image _ y) -> y) (\(Image x _) y -> Image x y)

getFrom :: (Eq a) => a -> Map a b -> Maybe (Image a b)
getFrom k (Map ((i@(Image x y)) : m)) = if x == k then Just i else getFrom k (Map m)
getFrom _ _ = Nothing
getTo :: (Eq b) => b -> Map a b -> Maybe (Image a b)
getTo v (Map ((i@(Image x y)) : m)) = if y == v then Just i else getTo v (Map m)
getTo _ _ = Nothing

mAddHead :: Image a b -> (Map a b) -> Map a b
mAddHead i (Map m) = Map (i : m)

-- MAP with default
mapFrom :: (Eq a) => a -> Maybe (Image a b) -> (Image a b -> Image a b) -> (Map a b) -> (Map a b)
mapFrom k i' f (Map ((i@(Image x y)) : m)) = if k == x then Map ((f i) : m) else mAddHead i (mapFrom k i' f (Map m))
mapFrom k (Just i') n (Map []) = Map (pure i')
mapFrom k Nothing n (Map []) = Map []
mapTo :: (Eq b) => b -> Maybe (Image a b) -> (Image a b -> Image a b) -> (Map a b) -> (Map a b)
mapTo v i' f (Map ((i@(Image x y)) : m)) = if v == y then Map ((f i) : m) else mAddHead i (mapTo v i' f (Map m))
mapTo v (Just i') f (Map []) = Map (pure i')
mapTo v (Nothing) f (Map []) = Map []
type instance Lens.Index (Map a b) = a
type instance Lens.IxValue (Map a b) = b

mAtGet1 :: (Eq k) => k -> (Map k v -> [Image k v])
mAtGet1 k (fromMap -> m) = (uncurry Image) <$> (filter ((== k) . fst) m)
mAtSet1 :: (Eq k) => k -> (Map k v -> [Image k v] -> Map k v)
mAtSet1 k (fromMap -> m) l =
  let newImages = (uncurry Image) <$> (filter (not . (== k) . fst) m)
   in (toMapImage newImages) <> (toMapImage l)
mAtGet2 :: (Eq v) => v -> (Map k v -> [Image k v])
mAtGet2 k (fromMap -> m) = (uncurry Image) <$> (filter ((== k) . snd) m)
mAtSet2 :: (Eq v) => v -> (Map k v -> [Image k v] -> Map k v)
mAtSet2 k (fromMap -> m) l =
  let newImages = (uncurry Image) <$> (filter (not . (== k) . snd) m)
   in (toMapImage newImages) <> (toMapImage l)

-- | Lens at key
mAt :: (Eq k) => k -> Lens.Lens' (Map k v) [Image k v]
mAt k = lens (mAtGet1 k) (mAtSet1 k)

-- | Lens at value
mAtV :: (Eq v) => v -> Lens.Lens' (Map k v) [Image k v]
mAtV v = lens (mAtGet2 v) (mAtSet2 v)

seeMapImg :: Lens.Iso' (Map k v) [Image k v]
seeMapImg = Lens.iso fromMapImage (\i -> toMapImage i)
seeImgTup :: Lens.Iso' (Image k v) (k, v)
seeImgTup = Lens.iso (\(Image k v) -> (k, v)) (\(k, v) -> (Image k v))
