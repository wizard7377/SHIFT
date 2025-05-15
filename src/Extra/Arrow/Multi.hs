module Extra.Arrow.Multi where

import Control.Arrow ((>>>))
import Control.Arrow qualified as A
import Control.Category qualified as C
import Data.Kind
import Data.List (singleton)
import Extra.Arrow.List
import Extra.Arrow.Trans (ArrowTrans (..))

listCase :: [a] -> Either () (a, [a])
listCase [] = Left ()
listCase (x : xs) = Right (x, xs)
newtype ArrowMultiT (t :: k0 -> Type -> Type) (a :: k0) (b :: Type) = ArrowMulti {runMultiT :: t a [b]}
instance (ArrowList cat, A.Arrow cat, C.Category cat) => C.Category (ArrowMultiT cat) where
  id :: ArrowMultiT cat a a
  id = ArrowMulti $ A.arr $ C.id >>> singleton
  (.) :: ArrowMultiT cat b c -> ArrowMultiT cat a b -> ArrowMultiT cat a c
  (ArrowMulti g) . (ArrowMulti f) =
    let
      a0 = f >>> arrL id >>> g
     in
      ArrowMulti a0

{-
instance (ArrowList ar) => A.Arrow (ArrowMultiT ar) where
  arr :: (ArrowList ar) => (b -> c) -> ArrowMultiT ar b c
  arr = liftArrow . A.arr
  (***) ::
    (ArrowList ar) =>
    ArrowMultiT ar b c ->
    ArrowMultiT ar b' c' ->
    ArrowMultiT ar (b, b') (c, c')
  (ArrowMulti f) *** (ArrowMulti g) = ArrowMulti $ (f A.*** g) >>> A.arr (uncurry zip)
-}
instance (A.Arrow ar, ArrowList ar) => ArrowTrans ArrowMultiT ar where
  liftArrow :: (A.Arrow ar) => ar b c -> ArrowMultiT ar b c
  liftArrow f = ArrowMulti $ f >>> A.arr singleton
instance (ArrowList ar) => ArrowList (ArrowMultiT ar) where
  arrL :: (ArrowList ar) => (a -> [b]) -> ArrowMultiT ar a b
  arrL = ArrowMulti . A.arr
  mapL ::
    (ArrowList ar) =>
    ([b] -> [c]) ->
    ArrowMultiT ar a b ->
    ArrowMultiT ar a c
  mapL f (ArrowMulti ar) = ArrowMulti $ ar >>> arrL (singleton . f)
