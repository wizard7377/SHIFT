module Extra.Map where

import Extra.Basics (implies, iso)

-- | A projection of a value of type @a@ onto a value of type @b@
type Image a b = (a, b)

type HImage a = Image a a

-- | A colelction of projections forming a mapping from @a@ to @b@
type Map a b = [Image a b]

-- | A mapping from @a@ to itself
type HMap a = Map a a

image :: a -> b -> Image a b
image a b = (a, b)
(>->) :: a -> b -> Image a b
(>->) = image
mlookup :: (Eq a) => Map a b -> a -> Maybe b
mlookup [] _ = Nothing
mlookup ((k, v) : xs) x = if k == x then Just v else mlookup xs x
mlookupV :: (Eq b) => Map a b -> b -> Maybe a
mlookupV ((k, v) : xs) y = if v == y then Just k else mlookupV xs y
mlookupV [] _ = Nothing
mapToF :: (Eq a) => HMap a -> a -> a
mapToF m v = case mlookup m v of
  Just x -> x
  Nothing -> v

miso :: (Eq a) => HMap a -> Bool
miso = iso

msuject :: (Eq a) => (Eq b) => Map a b -> Bool
msuject m = all (\(k, v) -> all (\(k', v') -> implies (k == k') (v == v')) m) m
minject :: (Eq a) => (Eq b) => Map a b -> Bool
minject m = all (\(k, v) -> all (\(k', v') -> implies (v == v') (k == k')) m) m

isFrom :: (Eq a, Eq b) => Image a b -> a -> Bool
isFrom (x, _) val = x == val
isTo :: (Eq a, Eq b) => Image a b -> b -> Bool
isTo (_, y) val = y == val

getKeys :: Map a b -> [a]
getKeys = map fst

getValues :: Map a b -> [b]
getValues = map snd
