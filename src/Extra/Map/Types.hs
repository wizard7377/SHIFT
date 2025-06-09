module Extra.Map.Types (
  --
  -- $imageTypes
  Image (..),
  HImage,
  Map (..),
  HMap,
) where

import Data.Bifoldable (Bifoldable (..))
import Data.List (intercalate)
import Extra.Basics
import Extra.Exports
import Extra.Lens

{- $imageTypes
The types of images (that is, pairs of `key` `value`)
-}

-- | A projection of a value of type @a@ onto a value of type @b@
data Image a b = Image a b
  deriving (Eq, Ord, Data, Typeable, Generic)

type HImage a = Image a a

data MapTagged t a b = MapTagged [(Image a b, t)]

-- | A colelction of projections forming a mapping from @a@ to @b@
data Map a b = Map [Image a b]
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | A mapping from @a@ to itself
type HMap a = Map a a

instance (Show a, Show b) => Show (Image a b) where
  show (Image x y) = show x ++ " |-> " ++ show y

instance (Show a, Show b) => Show (Map a b) where
  show (Map xs) = "{" ++ intercalate ", " (map show xs) ++ "}"

instance (Functor (Image a)) where
  fmap f (Image a b) = Image a (f b)
instance (Bifunctor Image) where
  bimap f g (Image a b) = Image (f a) (g b)
