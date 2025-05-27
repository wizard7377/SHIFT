{- |
 - Extra lens imports
 -
-}
module Extra.Lens (
  (^.),
  (^@.),
  (.~),
  (.=),
  (&),
  (<&>),
  Lens.each,
  Lens._1,
  Lens._2,
  Lens.Lens,
  Lens.Lens',
  Lens.makeLenses,
  Lens.makePrisms,
  Lens.Prism,
  Lens.Traversal,
  Lens.Plated,
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators
