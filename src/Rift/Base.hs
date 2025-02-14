module Rift.Base where 

import Text.Show.Functions()
type Token t = (Eq t,Show t)
type TPureComp a b = (a -> b)
-- |The type of all terms, parameterized over a given token type
data Term tok where
    Atom :: tok -> Term tok -- ^Atomic forms
    List :: [Term tok] -> Term tok -- ^Basic expressions expressions
    PureComp :: (Term tok -> Term tok) -> (Term tok) -> (Term tok) -- ^A pure computation 
    ImpureComp :: Monad m => (Term tok -> m (Term tok)) -> (Term tok) -> (Term tok) -- ^An impure computation, can transform to another type
    Lamed :: (Term tok) -> (Term tok) -> (Term tok) -- ^A lamed term
    Yud :: Term tok -- ^Truth
    Resh :: Term tok -- ^Falsehood
    Rule :: (Term tok) -> (Term tok) -> (Term tok) -- ^A subtyping rule of the form (to : from)
    Tagged :: tag -> Term tok -> Term tok -- TODO a bunch of stuff related to this

