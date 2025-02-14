module Rift.Unify(
    collectWith,collectUnify,trivialUnify,
    Unify(..),
    trivial,absurd,atomic,oneOf,bothOf
) where

import Rift.Base
import Test.QuickCheck (collect)
import Data.List (intersect,union)

data Unification a =
    Trivial
    | Absurd
    | Atomic a a
    | OneOf (Unification a) (Unification a)
    | BothOf (Unification a) (Unification a)

-- |Optimzing constructors for unification
trivial = Trivial
absurd = Absurd
atomic = Atomic
oneOf t0 t1 =
    case (t0,t1) of
        (Trivial,_) -> Trivial
        (_,Trivial) -> Trivial
        (Absurd,Absurd) -> Absurd
        (_,_) -> OneOf t0 t1
bothOf t0 t1 =
    case (t0,t1) of
        (Absurd,_) -> Absurd
        (_,Absurd) -> Absurd
        (Trivial,Trivial) -> Trivial
        (_,_) -> BothOf t0 t1


-- |Collect all possible solutions to the unification that only bind the first argument 
collectWith ::
    -- |The variable to search for bindings for 
    Eq a => (a -> Bool) ->
    -- |The unification to search
    Unification a ->
    -- |The result, note that the list is a sum (ie, or) of unifications, so the longer the list the more possibilites there are, with an empty list denotating that the variable is free. 
    -- Note that `Just []` `Nothing` have completly different meanings, with `Just []` meaning that the variable is free, while `Nothing` means that it is inconsistently bound 
    Maybe [a]
collectWith isVar term =
    case term of
        Trivial -> Just []
        Absurd -> Nothing
        Atomic t0 t1 -> if isVar t0 then Just [t1] else Nothing
        OneOf u0 u1 -> case (collectWith isVar u0, collectWith isVar u1) of {
            (Nothing,Nothing) -> Nothing ;
            (Just l0,Just l1) -> Just $ union l0 l1 ;
            (v,Nothing) -> v ;
            (Nothing,v) -> v
        }
        BothOf u0 u1 -> case (collectWith isVar u0, collectWith isVar u1) of {
            (Just l0,Just l1) -> Just $ intersect l0 l1 ;
            (_,_) -> Nothing
        }
collectUnify :: Eq a => [a] -> Unification a -> Maybe [a]
collectUnify l = collectWith (`elem` l)
trivialUnify :: Eq a => Unification a -> Bool 
trivialUnify t = case collectWith (const True) t of 
    Just [] -> True 
    _ -> False
class Unify a where
    unify :: a -> a -> Unification a

