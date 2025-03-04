module Extra.Basics where

implies :: Bool -> Bool -> Bool
implies a b = a || not b

(<$$>) :: (Functor f0) => (Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap
type Mapping a b = [(a, b)]

iso :: (Eq a) => (Eq b) => Mapping a b -> Bool
iso mapping = (all ((\(x0, y0) -> all (\(x1, y1) -> implies (x0 == x1) (y0 == y1)) mapping))) mapping
