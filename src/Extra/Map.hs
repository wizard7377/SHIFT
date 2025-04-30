{-# LANGUAGE AllowAmbiguousTypes #-}

module Extra.Map where

import Data.Bifunctor (Bifunctor (..))

implies :: Bool -> Bool -> Bool
implies a b = a || not b
iso :: (Eq a, Eq b) => [(a, b)] -> Bool
iso m = all (\(k, v) -> all (\(k', v') -> implies (k == k') (v == v')) m) m

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
infixl 2 @>
infixl 2 >@

-- | Map to function, first as input, second as output
mapToF :: (Eq a) => HMap a -> a -> a
mapToF m v = case mlookup m v of
  Just x -> x
  Nothing -> v

-- | Map to function, second as input, first as output
mapToFR :: (Eq a) => HMap a -> a -> a
mapToFR m v = case mlookupV m v of
  Just x -> x
  Nothing -> v

(@>) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
(>@) :: (Bifunctor b) => (Functor f) => (Eq a) => HMap a -> f (b a a) -> f (b a a)
mapping >@ value = fmap (first (mapToFR mapping)) value
mapping @> value = fmap (second (mapToF mapping)) value
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
flipflop :: Map a b -> Map b a
flipflop vals = (\(x, y) -> (y, x)) <$> vals
