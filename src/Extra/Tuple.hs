module Extra.Tuple where

type TwoWay a = (a, a)

-- | FF FT TF TT
type FourWay a = (a, a, a, a)

split :: (a -> Bool) -> [a] -> TwoWay [a]
split f l = (filter f l, filter (not . f) l)
split4 :: (a -> Bool) -> (a -> Bool) -> [a] -> FourWay [a]
split4 f0 f1 (x : xs) = split4' f0 f1 x <> split4 f0 f1 xs
split4 _ _ [] = ([], [], [], [])
split4' :: (a -> Bool) -> (a -> Bool) -> a -> FourWay [a]
split4' f0 f1 val =
  case (f0 val, f1 val) of
    (False, False) -> ([val], [], [], [])
    (False, True) -> ([], [val], [], [])
    (True, False) -> ([], [], [val], [])
    (True, True) -> ([], [], [], [val])
both :: (Monoid m) => (a -> m) -> (a, a) -> m
both f (x, y) = f x <> f y
