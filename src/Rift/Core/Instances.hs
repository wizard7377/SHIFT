module Rift.Core.Instances where

import Rift.Core.Base

{-
instance Token a => Unify (Term a) where
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

instance (Show box) => Show (Term box) where
  show (Atom a) = show a
  show (BCons Cons l0 l1) = show l0 ++ " . " ++ show l1
  show (BCons Lamed b t) = "?" ++ show b ++ "#[" ++ show t ++ "]#"
  show Yud = "*"
  show (BCons Rule t f) = "#{" ++ show t ++ " : " ++ show f ++ "}#"
  -- show (Empty) = "()"
  show _ = "!"
