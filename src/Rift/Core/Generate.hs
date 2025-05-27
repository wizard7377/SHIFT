module Rift.Core.Generate where

import Rift.Core.Interface (UTermLike (..))

newtype AGenerator gen = Generator gen
class Generator gen val where
  generate :: gen -> (val, gen)

instance (Num a) => Generator (AGenerator a) a where
  generate (Generator gen) = (gen, Generator $ gen + 1)

generateTerm :: (Generator gen tag) => (UTermLike term tag) => gen -> term -> (term, gen)
generateTerm gen term =
  let (tag, newGen) = generate gen
   in (uniqueCreate term tag, newGen)
