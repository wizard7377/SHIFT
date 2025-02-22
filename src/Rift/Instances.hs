module Rift.Instances where

import Rift.Base
import Rift.Unify
import Extra.Choice
import Extra.Choice (Choice(EmptyNode))

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
instance Show a => Show (Term a) where
    show (Atom a) = show a
    show (List l) = "#(" ++ showList l "" ++ ")#"
    show (PureComp _ v) = "<>" ++ show v
    show (ImpureComp _ v) = "<!>" ++ show v
    show (Lamed b t) = "?" ++ show b ++ "#[" ++ show t ++ "]#"
    show Yud = "*"
    show Resh = "!"
    show (Rule t f) = "#{" ++ show t ++ " : " ++ show f ++ "}#"
    show (Tagged tag t) = "+" ++ show t
    show (He t) = "$" ++ (show t)

instance Eq a => Eq (Term a) where
    (Atom a0) == (Atom a1) = a0 == a1
    (List l0) == (List l1) = l0 == l1
    (Lamed b0 t0) == (Lamed b1 t1) = (b0 == b1) && (t0 == t1)
    Yud == Yud = True
    Resh == Resh = True
    (Rule t0 f0) == (Rule t1 f1) = (t0 == t1) && (f0 == f1)
    (Tagged _ t0) == t1 = t0 == t1
    t0 == (Tagged _ t1) = t0 == t1
    He t0 == He t1 = t0 == t1
    _ == _ = False


