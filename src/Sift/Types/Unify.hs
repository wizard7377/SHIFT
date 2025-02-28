module Sift.Types.Unify where

import Data.List
import Extra.List
import Rift (Term (..), unify, (>?>), (>@>))
import Rift.Funcs
import Sift.Base (QTerm, SAtom (..), STerm)

intros :: STerm atom -> QTerm atom
intros (Lamed bound term) =
  let
    (oterm, obound) = intros term
    allbounds = bound : obound
    newterm = forEach (\bound acc -> replace bound (He bound) acc) oterm allbounds
   in
    (newterm, allbounds)
intros term = (term, [])

unintros :: QTerm atom -> STerm atom
unintros (term, []) = term
unintros (term, bound : xs) =
  let
    termA = unintros (term, xs)
    termB = replace (He bound) bound termA
   in
    (Lamed bound termB)
