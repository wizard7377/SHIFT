module Extra.Tuple where 

both :: Monoid m => (a -> m) -> (a,a) -> m 
both f (x,y) = f x <> f y  