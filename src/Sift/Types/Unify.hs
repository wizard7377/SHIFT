module Sift.Types.Unify where

import Data.List
import Data.Map
import Extra.Basics
import Extra.List
import Rift
import Rift (Atomic, Term (..), unify)
import Sift.Base (QTerm, SAtom (..), STerm, fromSTerm, toSTerm)

intros :: (Atomic atom) => Term atom -> QTerm atom
intros (Lamed bound term) =
  let
    (oterm, obound) = intros term
    allbounds = bound : obound
    newterm = forEach (\bound acc -> replace bound (Atom $ He $ fromSTerm bound) acc) allbounds oterm
   in
    (newterm, allbounds)
intros term = (term, [])

unintros' :: (Atomic atom) => [Term atom] -> Term atom -> Term atom
unintros' frees within = unintros (within, frees)
unintros :: (Atomic atom) => QTerm atom -> Term atom
unintros (term, []) = term
unintros (term, bound : xs) =
  let
    termA = unintros (term, xs)
    termB = replace bound bound termA
   in
    (Lamed bound termB)
