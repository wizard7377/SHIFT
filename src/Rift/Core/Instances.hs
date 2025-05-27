{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}

module Rift.Core.Instances where

import Data.List (intercalate, intersperse)
import Data.Text qualified as T
import Extra
import Extra.Debug
import Rift.Core.Base
import Rift.Core.Interface
import Rift.Core.Kernel
import Rift.Core.Unify.Util
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

showRainbow :: (Show a) => Int -> Term' a -> [Char]
showRainbow i (Lamed v b t) = showColor' i "[" ++ showRainbow n v ++ showColor' i "] {" ++ showRainbow n b ++ " " ++ showRainbow n t ++ showColor' i "}"
 where
  n = i + 1
showRainbow i x@(Cons a0 a1) = showColor' i "(" ++ intercalate (showColor' i " ") (showRainbow n <$> parseCons x) ++ showColor' i ")"
 where
  n = i + 1
showRainbow i (Atom atom) = resetCode ++ show atom
 where
  n = i + 1
showRainbow i BasicLamed = resetCode ++ "ל"
showListT :: (Show a) => [Term' a] -> String
showListT v = if null v then "{}" else "\n[\n\t" ++ intercalate "\n\t" (showRainbow 1 <$> v) ++ "\n]"

instance (Show TestToken) => Show TestTerm where
  show = showRainbow 1

-- showList v = (++ showListT v)
instance UTermLike TestTerm Int where
  uniqueCreate term tag = PrimAtom $ TestLogicToken tag

type TermFull tag term = (Term term, TermLike term, FTermLike term, UTermLike term tag)
