{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Rift.Core.Instances where

import Control.Lens (lens)
import Control.Lens qualified as Lens
import Data.List (intercalate, intersperse)
import Data.Text qualified as T
import Extra
import Extra.Debug
import Rift.Core.Base
import Rift.Core.Interface
import Rift.Core.Kernel
import Rift.Core.Utility
import System.Console.ANSI qualified as ANSI

{-
instance Token a => Unify (Term' a) where
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

instance {-# OVERLAPPING #-} Show TestToken where
  show (TestToken (Left t)) = T.unpack t
  show (TestToken (Right t)) = "_" ++ show t
  show (TestLogicToken t) = "$" ++ show t

showRainbow :: (Show a, Show (Term' a)) => Int -> Term' a -> [Char]
showRainbow i (PrimTag t tag) = showColor' i (showRainbow n t ++ "@" ++ show tag)
 where
  n = i + 1
showRainbow i (Lamed v b t f) = showColor' i "[" ++ showRainbow n v ++ showColor' i "] {" ++ showRainbow n b ++ " -> " ++ showRainbow n t ++ " ? " ++ showRainbow n f ++ showColor' i "}"
 where
  n = i + 1
showRainbow i (Kaf a0 a1) = showColor' i "(" ++ intercalate (showColor' i " ") (showRainbow n <$> parseCons a0) ++ " . " ++ (showRainbow n a1) ++ showColor' i ")"
 where
  n = i + 1
showRainbow i (PrimAtom atom) = resetCode ++ show atom
 where
  n = i + 1
showRainbow i BasicLamed = resetCode ++ "ל"
showRainbow i (PrimFree t v) = showColor' i "<" ++ intercalate (showColor' i ", ") (showRainbow n <$> v) ++ showColor' i ">" ++ " " ++ showRainbow n t
 where
  n = i + 1
showListT :: (Show a, Show (Term' a)) => [Term' a] -> String
showListT v = if null v then "{}" else "\n[\n\t" ++ intercalate "\n\t" (showRainbow 1 <$> v) ++ "\n]"

instance (Show TestToken) => Show TestTerm where
  show t = showRainbow 1 t

-- showList v = (++ showListT v)
instance UTerm Idx TestTerm where
  uniqueCreate term tag = PrimTag term tag

freesIn :: Lens' (Term' atom) [(Term' atom)]
freesIn =
  lens
    ( \case
        (PrimFree _ v) -> v
        _ -> []
    )
    ( \term v ->
        case term of
          (PrimFree t _) -> PrimFree t v
          _ -> PrimFree term v
    )
termIn :: Lens' (Term' atom) (Term' atom)
termIn =
  lens
    ( \case
        (PrimFree t v) -> t
        t -> t
    )
    ( \term term' ->
        case term of
          (PrimFree t v) -> PrimFree term' v
          _ -> term'
    )
instance FTerm (Term' atom) where
  type Inner (Term' atom) = (Term' atom)
  fterm = termIn
  frees = freesIn
  groundTerm = id

instance ETerm (Term' atom) where
  eterm = PrimError
type TermFull tag term = (KTerm term, TermLike term, FTerm term, UTerm tag term)
instance (Eq atom) => RTerm (Term' atom) where
  replaceTerm x y = Lens.transform (change x y)
