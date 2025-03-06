module Rift.Core.Dev.Check where

import Rift.Core.Base
import Test.QuickCheck

abin :: (Arbitrary a, Arbitrary b) => (a -> b -> c) -> Gen c
abin = applyArbitrary2
arbitraryTerm :: (Arbitrary a) => Gen (Term a)
arbitraryTerm =
  frequency
    [ (2, abin Cons)
    , (2, abin Rule)
    , (2, abin Lamed)
    , (5, Atom <$> arbitrary)
    , (1, return Yud)
    , (2, return Empty)
    ]
instance (Arbitrary a) => Arbitrary (Term a) where
  arbitrary = arbitraryTerm
  shrink = genericShrink
