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
  (^..),
  (<>~),
  Lens.each,
  Lens._1,
  Lens._2,
  Lens._3,
  Lens._4,
  Lens._5,
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
  Lens.Plated.Plated (..),
  Lens.transform,
  Lens.universe,
  Lens.cosmos,
) where

import Control.Lens qualified as Lens
import Control.Lens.Operators
import Control.Lens.Plated qualified as Lens.Plated
