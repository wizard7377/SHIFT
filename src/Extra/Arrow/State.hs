module Extra.Arrow.State where

newtype StateArrowT s a b c = StateArrow {runStateArrow :: a (b, s) (c, s)}
