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
  (^?),
  Lens.each,
  Lens._1,
  Lens._2,
  Lens.Lens,
  Lens.Lens',
  Lens.makeLenses,
  Lens.makePrisms,
  Lens.Prism,
  Lens.Prism',
  Lens.Traversal,
  Lens.Traversal',
  Lens.Plated,
  Lens.traversal,
  Lens.preview,
  Lens.review,
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators
