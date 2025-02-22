module Extra.List where

forEach :: (b -> a -> a) -> [b] -> a -> a 

forEach func (x:xs) val = 
    forEach func xs $ func x val
forEach _ [] val = val

