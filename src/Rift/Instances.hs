module Rift.Instances where 

import Rift.Base 
import Rift.Unify

instance Token a => Unify (Term a) where 
    unify t0 t1 = oneOf (atomic t0 t1) $ case (t0,t1) of {

    }

instance Show a => Show (Term a) where 
    show (Atom a) = show a
    show (List l) = "(" ++ (concat $ show <$> l) ++ ")"
    show (PureComp _ v) = "<>" ++ show v
    show (ImpureComp _ v) = "<!>" ++ show v 
    show (Lamed b t) = "?" ++ show b ++ "[" ++ show t ++ "]" 
    show Yud = "*"
    show Resh = "!"
    show (Rule t f) = "{" ++ show t ++ " : " ++ "}"
    show (Tagged tag t) = "+" ++ show t

