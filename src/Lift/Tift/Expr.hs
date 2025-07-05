{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}

module Lift.Tift.Expr where
import Rift.Core.Kernel
import Control.Monad.Identity (Identity)
import Data.Data (Data)
import Data.Kind (Constraint, Type)
import Data.Text as T hiding (cons)
import GHC.Generics (Generic)
import Text.Show.Functions ()
import Extra
import Data.Type.Equality ((:~:) (..))
import qualified Control.Lens as Lens
import Rift.Core.Interface
import Rift.Core.Classes
import Rift.Core.Base hiding (recurseManyB, recurseManyA, recurseSome, listToTerm, termToList, manyLamed, Lamed, poccurs)
import Control.Lens (lens)
import Extra.Color
import qualified Data.List as String
-- |The type of terms used for testing `Sift`
data TiftTerm where
  -- |ל
  TiftLamed :: TiftTerm
  -- |ט
  TiftAtom :: T.Text -> TiftTerm
  -- |כ
  TiftCons :: TiftTerm -> TiftTerm -> TiftTerm
  TiftTag :: TiftTerm -> Int -> TiftTerm
  -- |ד
  TiftFree :: TiftTerm -> [TiftTerm] -> TiftTerm
  -- |`λfrom . λto . λterm . (term[from := to])`
  TiftRep :: TiftTerm -> TiftTerm -> TiftTerm -> TiftTerm
  TiftError :: TiftTerm
  TiftWild :: Int -> TiftTerm
  TiftPe :: Idx -> TiftTerm -> TiftTerm
  TiftFe :: Idx -> TiftTerm
  deriving (Data,Typeable,Generic)

instance Plated TiftTerm where 
  plate f (TiftCons a0 a1) = TiftCons <$> f a0 <*> f a1
  plate f (TiftTag a n) = TiftTag <$> f a <*> pure n
  plate f (TiftFree a vs) = TiftFree <$> f a <*> pure vs
  plate f (TiftRep a0 a1 a2) = TiftRep <$> f a0 <*> f a1 <*> f a2
  plate f (TiftPe i a) = TiftPe i <$> f a
  plate f (TiftFe i) = pure (TiftFe i)
  plate f (TiftAtom a) = pure (TiftAtom a)
  plate f (TiftLamed) = pure TiftLamed
  plate f (TiftWild n) = pure (TiftWild n)
  plate f TiftError = pure TiftError
  plate f t = pure t
instance Eq TiftTerm where
  TiftLamed == TiftLamed = True
  TiftAtom a == TiftAtom b = a == b
  TiftCons a0 a1 == TiftCons b0 b1 = (a0 == b0) && (a1 == b1)
  TiftTag a n == TiftTag b m = (a == b) && (n == m)
  TiftFree a vs == TiftFree b ws = (a == b) && (vs == ws)
  TiftRep a0 a1 a2 == TiftRep b0 b1 b2 =
    (a0 == b0) && (a1 == b1) && (a2 == b2)
  --TiftRep a0 a1 a2 == t = if (occurs a0 a1) then False else ((Lens.transform (change a0 a1) a2) == t)
  --t == TiftRep a0 a1 a2 = if (occurs a0 a1) then False else (t == Lens.transform (change a0 a1) a2)
  TiftError == TiftError = True
  -- TODO:
  TiftPe i a == TiftPe j b = i == j && a == b
  -- TODO:
  TiftFe i == TiftFe j = i == j
  TiftWild i == TiftWild j = i == j
  _ == _ = False
instance (KTerm (TiftTerm)) where
  isLamed TiftLamed = True
  isLamed _ = False
  mkLamed = TiftLamed
  pKaf = Lens.prism'
    (\(PrimKafCon a b) -> TiftCons a b)
    (\case
      TiftCons a b -> Just (PrimKafCon a b)
      _ -> Nothing)


{- | The attomic class constraint
Represents a collection of "packaged requirements" that all atoms must have
All of these are fairly standard and should be implemented for most types anyway
-}
type Atomic a = (Eq a, Ord a, Show a)
-- |The functional version of `TiftAtom`, equavilent to `Atom`

{-# DEPRECATED #-}
mkCons3 :: AnyTerm term => term->term->term-> term
mkCons3 f a0 a1 = Kaf f (Kaf a0 a1)

{-# DEPRECATED #-}
mkCons :: AnyTerm term => term->term-> term
mkCons = Kaf
-- |The functional version of `Kaf`, equaivelent to `Kaf`
cons :: AnyTerm term => term->term-> term
cons = Kaf

-- |The functional version of `TiftLamed`, equaivelent to `Lamed`
lamed ::
  KTerm term =>
  -- |The variable
  term ->
  -- |The consequent
  term ->
  -- |The precedent
  term ->
  -- |The result
  term
lamed frees a0 a1 = Kaf BasicLamed (Kaf frees (Kaf a0 a1))

-- FOR TESTS ONLY, THIS IS A PARTIAL FUNCTION
manyLamed :: KTerm term => [term] -> term -> term -> term -> term
manyLamed [t] a0 a1 a2 = Lamed t a0 a1 a2
manyLamed (t : ts) a0 a1 a2 = Lamed t (manyLamed ts a0 a1 a2) a1 a2
--drule = mkCons' ARule


pattern Cons3 :: KTerm term => term -> term -> term -> term
pattern Cons3 f a0 a1 <- Kaf f (Kaf a0 a1)
  where
    Cons3 f a0 a1 = Kaf f (Kaf a0 a1)

pattern Lamed :: KTerm term => term -> term -> term -> term -> term
pattern Lamed v a b f <- Kaf BasicLamed (Kaf v (Kaf a (Kaf b f)))
  where
    Lamed v a b f = Kaf BasicLamed (Kaf v (Kaf a (Kaf b f)))
{-# INLINE Lamed #-}
termToList :: KTerm term => term -> [term]
termToList (Kaf a0 a1) = (:) a0 $ termToList a1
termToList _ = []

listToTerm :: AnyTerm term => [term] -> Maybe (term)
listToTerm [] = Nothing
listToTerm [x] = Just x
listToTerm (x : xs) = Kaf x <$> listToTerm xs

-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseSome :: (Eq term, KTerm term) => (term -> term) -> term -> term
recurseSome f v = let v' = f v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseSome f a0) (recurseSome f a1)
      Atom _ -> v'
      BasicLamed -> BasicLamed
  )
    else v'
-- |For some mapping, recursivly apply it, and only stop if we reach the bottom or the mapping changes the value
recurseManyA :: (Eq term, KTerm term) => (term -> term) -> (term -> term) -> term -> term
recurseManyB :: (Eq term, KTerm term) => (term -> term) -> (term -> term) -> term -> term
recurseManyA f g v = let v' = f v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseManyA f g a0) (recurseManyA f g a1)
      _ -> v'
  )
    else recurseManyB f g v'
recurseManyB f g v = let v' = g v in
  if v == v' then (
    case v' of
      Kaf a0 a1 -> Kaf (recurseManyB f g a0) (recurseManyB f g a1)
      _ -> v'
  )
    else recurseManyA f g v'
-- |Check if a given term occurs within another term
poccurs :: (Eq term, KTerm term) =>
  -- |The larger term
  term ->
  -- |The smaller term
  term -> Bool
poccurs term value =
  (value == term) || (case term of
    Kaf a0 a1 -> (poccurs term a0) || (poccurs term a1)
    Atom _ -> False
    BasicLamed -> False)


{-
instance Token a => Unify (TiftTerm) where
    unify ta tb = AnyOf [Simple (ta, tb), case (ta,tb) of {
        (Atom a0,Atom a1) -> if a0 == a1 then Trivial else EmptyNode ;
        ((List (h0:t0)),(List (h1:t1))) -> AllOf [(unify h0 h1), (unify (List t0) (List t1))] ;
        (Lamed b0 t0,Lamed b1 t1) -> AllOf [(unify b0 b1), (unify t0 t1)] ;
        (Yud,Yud) -> Trivial ;
        (Resh,Resh) -> Trivial ;
        (Rule t0 f0,Rule t1 f1) -> AllOf [(unify t0 t1), (unify f0 f1)] ;
        (Tagged _ t0,t1) -> unify t0 t1 ;
        (t0,Tagged _ t1) -> unify t0 t1 ;
        _ -> EmptyNode
    }]
-}

parseCons :: TiftTerm -> [TiftTerm]
parseCons (Kaf a0 a1) = a0 : parseCons a1
parseCons t = [t]
showRainbow :: (Show (TiftTerm)) => Int -> TiftTerm -> String
showRainbow i (TiftTag t tag) = showColor' i (showRainbow n t ++ "@" ++ show tag)
 where
  n = i + 1
showRainbow i (Kaf a0 a1) = showColor' i "(" ++ String.intercalate (showColor' i " ") (showRainbow n <$> parseCons a0) ++ " . " ++ (showRainbow n a1) ++ showColor' i ")"
 where
  n = i + 1
showRainbow i (TiftAtom atom) = resetCode ++ T.unpack atom
 where
  n = i + 1
showRainbow i BasicLamed = resetCode ++ "@lam"
showRainbow i (TiftRep from to within) = showColor' i "{" ++ (showRainbow n from) ++ " := " ++ (showRainbow n to) ++ showColor' i "}" ++ " " ++ showRainbow n within
 where
  n = i + 1
showRainbow i (TiftFree t v) = showColor' i "[" <> (String.intercalate (showColor' i ", ") (showRainbow n <$> v) <> (showColor' i "]" ++ (" " <> showRainbow n t)))
 where
  n = i + 1

showRainbow i (TiftWild n) = showColor' i "_" ++ show n
showRainbow i TiftError = showColor' i "⊥"
showRainbow i (TiftPe idx term) = showColor' i "@" ++ show idx ++ " {" ++ showRainbow n term ++ "}" 
 where
  n = i + 1

showRainbow i (TiftFe idx) = showColor' i "&" ++ show idx
showListT :: (Show (TiftTerm)) => [TiftTerm] -> String
showListT v = if Prelude.null v then "{}" else "\n[\n\t" ++ String.intercalate "\n\t" (showRainbow 1 <$> v) ++ "\n]"

instance Show TiftTerm where
  show t = showRainbow 1 t

-- showList v = (++ showListT v)
instance UTerm Idx TiftTerm where
  uniqueCreate term tag = TiftTag term tag

freesIn :: Lens' (TiftTerm) [(TiftTerm)]
freesIn =
  lens
    ( \case
        (TiftFree _ v) -> v
        _ -> []
    )
    ( \term v ->
        case (v, term) of
          ([], (TiftFree t _)) -> t
          (_, (TiftFree t _)) -> TiftFree t v
          (_, _) -> TiftFree term v
    )
termIn :: Lens' (TiftTerm) (TiftTerm)
termIn =
  lens
    ( \case
        (TiftFree t v) -> t
        t -> t
    )
    ( \term term' ->
        case term of
          (TiftFree t []) -> term'
          (TiftFree t v) -> TiftFree term' v
          _ -> term'
    )

simplify :: TiftTerm -> TiftTerm
simplify t = case t of
  TiftFree t' [] -> simplify t'
  TiftFree (TiftFree t' v0) v1 -> TiftFree (simplify t') (v0 <> v1)
  _ -> t
instance FTerm (TiftTerm) where
  type Inner (TiftTerm) = (TiftTerm)

  {-
  pDalet =
    Lens.prism'
      (\(TiftDaletCon t v) -> addFree v t)
      ( \case
          TiftFree t (v0 : []) -> Just (TiftDaletCon v0 t)
          TiftFree t (v0 : vs) -> Just (TiftDaletCon v0 (TiftFree t vs))
          _ -> Nothing
      )
  -}
  fterm = termIn
  frees = freesIn
  groundTerm = id

instance ETerm (TiftTerm) where
  eterm = TiftError
type TermFull tag term = (KTerm term, TermLike term, FTerm term, UTerm tag term)
instance RTerm (TiftTerm) where
  pAyin =
    Lens.prism'
      (\(PrimAyinCon from to within) -> TiftRep from to within)
      ( \case
          (simplify -> TiftRep from to within) -> Just (PrimAyinCon from to within)
          _ -> Nothing
      )
  replaceTerm x y = (TiftRep x y)
instance PTerm Idx (TiftTerm ) where
  seePe =
    Lens.prism'
      (\(PrimPeCon a t) -> TiftPe a t)
      ( \case
          TiftPe a t -> Just (PrimPeCon a t)
          _ -> Nothing
      )
  seeFe =
    Lens.prism'
      (\(a) -> TiftFe a)
      ( \case
          TiftFe a -> Just a
          _ -> Nothing
      )
