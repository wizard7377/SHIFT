{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Extra.Map.Types (
  --
  -- $imageTypes
  Image (..),
  HImage,
  Map,
  HMap,
  TMap (..),
  TImage (..),
  HasMapIndex (..),
  HasMapValue (..),
  HasMapTag (..),
) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bitraversable
import Data.Default qualified as D
import Data.List (intercalate)
import Extra.Basics
import Extra.Exports
import Extra.Lens

{- $imageTypes
The types of images (that is, pairs of `key` `value`)
-}

-- | A projection of a value of type @a@ onto a value of type @b@, with a tag @t@.
data TImage t a b = TImage t a b
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | A projection with unit tag.
type Image = TImage ()

-- | A projection from a type to itself, with unit tag.
type HImage a = Image a a

-- | A tagged map from @a@ to @b@.
data TMap t a b = TMap [TImage t a b]
  deriving (Eq, Ord, Data, Typeable, Generic)

-- | A collection of projections forming a mapping from @a@ to @b@.
type Map = TMap ()

-- | A mapping from @a@ to itself.
type HMap a = Map a a

instance Functor (TImage t a) where
  fmap f (TImage t a b) = TImage t a (f b)

instance (Bifunctor (TImage t)) where
  bimap f g (TImage t a b) = TImage t (f a) (g b)

instance Functor (TMap t a) where
  fmap f (TMap xs) = TMap $ map (\(TImage t a b) -> TImage t a (f b)) xs

instance Bifunctor (TMap t) where
  bimap f g (TMap xs) = TMap $ map (\(TImage t a b) -> TImage t (f a) (g b)) xs

instance Bifoldable (TImage t) where
  bifoldMap f g (TImage _ a b) = f a `mappend` g b

instance Bifoldable (TMap t) where
  bifoldMap f g (TMap xs) = foldMap (\(TImage t a b) -> f a `mappend` g b) xs

instance Bitraversable (TImage t) where
  bitraverse f g (TImage t a b) = TImage t <$> f a <*> g b

instance Bitraversable (TMap t) where
  bitraverse f g (TMap xs) = TMap <$> traverse (\(TImage t a b) -> TImage t <$> f a <*> g b) xs

instance D.Default (Map a b) where
  def = TMap []

-- | Typeclass for types with a map index.
class HasMapIndex m where
  type MapIndex m

-- | Typeclass for types with a map value.
class HasMapValue m where
  type MapValue m

-- | Typeclass for types with a map tag.
class HasMapTag m where
  type MapTag m

instance HasMapIndex (TMap t a b) where
  type MapIndex (TMap t a b) = a

instance HasMapValue (TMap t a b) where
  type MapValue (TMap t a b) = b

instance HasMapTag (TMap t a b) where
  type MapTag (TMap t a b) = t
instance {-# OVERLAPS #-} (Show a, Show b) => Show (Image a b) where
  show (TImage t x y) = show t ++ " : " ++ show x ++ " |-> " ++ show y

instance {-# OVERLAPS #-} (Show a, Show b) => Show (Map a b) where
  show (TMap xs) = "{" ++ intercalate ", " (map show xs) ++ "}"

instance {-# OVERLAPPABLE #-} (Show t, Show a, Show b) => Show (TImage t a b) where
  show (TImage t x y) = show x ++ " =" ++ show t ++ "= " ++ show y

instance (Show a, Show b, Show t) => Show (TMap t a b) where
  show (TMap xs) = "{" ++ intercalate ", " (map show xs) ++ "}"
