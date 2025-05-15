{-# LANGUAGE Arrows #-}

module Tests.Arrows where

import Control.Arrow (returnA)
import Extra.Arrow

t0 :: StateArrowT (Either String Int) (ArrowMultiT (->)) Int Int
t0 = proc x -> do
  y <- unlist -< [(+ 2), (* 3)]
  let r = y x
  returnA -< r
