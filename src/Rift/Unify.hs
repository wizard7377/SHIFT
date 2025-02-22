{-# LANGUAGE BlockArguments #-}
module Rift.Unify(
    (@=),(@?),goodUnify,
    Binding, Binding',Bindings
) where

import Rift.Base

import Test.QuickCheck (collect)
import Data.List (intersect,union)
import Extra.Choice
import Extra.List (forEach)
import Debug.Trace

-- |The type of a binding, just a tuple with the from @(from,to)@
type Binding' a b = (a,b)
-- |The type of a homogenues binding
type Binding a = Binding' a a
-- |A list of homogenuos bindings
type Bindings a = [Binding a]

{-|Collect the corresponding nodes on each tree, where it stops descending whenever the function is true

-}
unify :: (Term a -> Term b -> Bool) -> Term a -> Term b -> Maybe [Binding' (Term a) (Term b)]
unify f ta tb =
    if f ta tb then Just [(ta,tb)] else
        case (ta,tb) of {
            (Atom va,Atom vb) -> Just [(ta,tb)] ;
            (List (x:xs),List (y:ys)) -> Just [(x,y)] <> thisUnify (List xs) (List ys) ;
            (Lamed ba ta, Lamed bb tb) -> thisUnify ba bb <> thisUnify ta tb ;
            (Yud, Yud) -> Just [] ;
            (Resh,Resh) -> Just [] ;
            (Rule aa ba, Rule ab bb) -> thisUnify aa ab <> thisUnify ba bb ;
            (Tagged _ va, Tagged _ vb) -> thisUnify va vb ;
            (val@(He _),_) -> Just [(val,tb)] ;
            _ -> Nothing
        }
    where thisUnify = unify f


infix 5 @=
(@=) :: Term a -> Term b -> Maybe [Binding' (Term a) (Term b)]
v0 @= v1 = unify (\x y -> isFundamental x || isFundamental y) v0 v1
infix 5 @? 
(@?) :: Token (Term a) => Term a -> Term a -> Maybe [Binding' (Term a) (Term a)]
v0 @? v1 = filter (\(x,y) -> x /= y) <$> unify (\x y -> isFundamental x || isFundamental y || x == y) v0 v1

unique :: Token a => [a] -> Bool
unique vals = all (\v0 -> all (\v1 -> v0 == v1) vals) vals
lookupAll :: Token k => [(k,v)] -> k -> [v]
lookupAll vals k = snd <$> filter (\(k2,_) -> k == k2) vals
goodUnify :: Token (Term a) => Maybe [Binding' (Term a) (Term a)] -> Bool
goodUnify i = case i of {
    Nothing -> False ; 
    Just [] -> True ;
    Just binds -> traceShowId (all (\(k,_) -> 
        let vals = traceShowId $ lookupAll binds k in
            unique vals
    ) binds) && ((all (\(k,v) -> ((k == v) || isHe k))) binds) -- TODO something im forgetting im sure
}


