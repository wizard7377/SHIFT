{-# LANGUAGE BlockArguments #-}
module Rift.Funcs where

import Rift.Base
import Rift.Unify
import Extra.Choice
import Debug.Trace
import Rift.Instances()
import Data.Traversable (for)
import Extra.List
-- |Replace every instance of a given value with another
replace :: Token a =>
    -- |From 
    Term a ->
    -- |To  
    Term a ->
    -- |Within 
    Term a ->
    Term a

replace from to within | from == within = to
replace from to within = let myreplace = replace from to in case within of {
    Atom _ -> within ;
    List l -> List $ myreplace <$> l ;
    PureComp f t -> PureComp f $ myreplace t ;
    ImpureComp f t -> ImpureComp f $ myreplace t ;
    Lamed b t -> Lamed (myreplace b) (myreplace t) ;
    Yud -> within ;
    Resh -> within ;
    Rule t f -> Rule (myreplace t) (myreplace f) ;
    Tagged e t -> Tagged e (myreplace t) ;
    He val -> He $ myreplace val ;
}
-- |Gets all potentially free vars
intros :: Term a -> (Term a,[Term a])

intros (Lamed b t) = let (t1,b1s) = intros t in (t1,(b : b1s))
intros t = (t,[])

unintros :: Term a -> [Term a] -> Term a 
unintros term vars = case vars of {
    [] -> term ;
    (x:xs) -> unintros (Lamed x term) xs
}
addHe :: Token a => Term a -> Term a -> Term a
addHe called = replace called (He called)
introduce :: Token a => Term a -> Term a
introduce term =
    let (baseTerm,vars) = intros term in
        forEach addHe vars baseTerm


-- |"Shrink" the term, that is, generate a term that is as generally as possible equal
shrink :: Token a => Term a -> Term a -> Maybe (Term a, Bindings (Term a))
shrink termAt termBt = let 
    termA = introduce termAt
    termB = introduce termBt
    needs = termA @? termB 
    in if goodUnify needs then  
        case needs of 
            Just needsJ -> Just $ (forEach (\(k,v) t -> replace k v t) needsJ termA, needsJ)
            Nothing -> Nothing   
    else Nothing  
shrink_ :: Token a => Term a -> Term a -> Maybe (Term a)
shrink_ termAt termBt = fst <$> shrink termAt termBt
infixr 8 @> 
(@>) :: Token a => Term a -> Term a -> Maybe (Term a)
(@>) = shrink_
chain :: Bindings a -> [Term a] -> Maybe [Term a]

chain vars term = _