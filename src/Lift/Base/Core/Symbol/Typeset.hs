{-# LANGUAGE ExistentialQuantification #-}

module Lift.Base.Core.Symbol.Typeset where

import Extra

data Command a where
  Command :: (Functional f [a] [a]) => f -> Command a

class (Monoid a) => Typeset a where
  typeset :: Command a -> a
