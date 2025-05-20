module Extra.Monad where

import Control.Monad ((>=>))

recurseM :: (Monad m) => Int -> (a -> m a) -> (a -> m a)
recurseM 0 _ = pure
recurseM n f = f >=> (recurseM (n - 1) f)
