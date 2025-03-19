module Extra.List where

import Data.Bifunctor

-- | Apply a function to each element of a list
forEach :: (b -> a -> a) -> [b] -> a -> a
forEach func (x : xs) val =
  forEach func xs $ func x val
forEach _ [] val = val

-- | Takes a list, and returns a list of pairs of a subsequence of a list and all things not in said subsequence
subParts :: [a] -> [([a], [a])]
subParts [] = [([], [])]
subParts (x : xs) =
  let
    subRest = subParts xs
    subLeft = first (x :) <$> subRest
    subRight = second (x :) <$> subRest
   in
    subLeft <> subRight
