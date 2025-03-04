module Sift.Types.Unify where

import Data.List
import Data.Map
import Extra.Basics
import Extra.List
import Rift (Atomic, Term (..), unify, (>?>), (>@>))
import Rift.Funcs
import Rift.Unify
import Sift.Base (QTerm, SAtom (..), STerm, fromSTerm, toSTerm)

isHe :: SAtom atom -> Bool
isHe (He _) = True
isHe _ = False
intros :: (Atomic atom) => STerm atom -> QTerm atom
intros (Lamed bound term) =
  let
    (oterm, obound) = intros term
    allbounds = bound : obound
    newterm = forEach (\bound acc -> replace bound (Atom $ He $ fromSTerm bound) acc) allbounds oterm
   in
    (newterm, allbounds)
intros term = (term, [])

unintros :: (Atomic atom) => QTerm atom -> STerm atom
unintros (term, []) = term
unintros (term, bound : xs) =
  let
    termA = unintros (term, xs)
    termB = replace bound bound termA
   in
    (Lamed bound termB)

isBindIn :: (Atomic atom) => [STerm atom] -> STerm atom -> Bool
isBindIn binds term = elem term binds
allowed :: (Atomic atom) => [STerm atom] -> Unification Term (SAtom atom) -> Bool
allowed frees unification = (all (isBindIn frees) (keys unification)) && (iso $ assocs unification)
