module Rift.Core.Utility where

import Rift.Core.Base
import Rift.Core.Kernel

parseLamed :: (Term term) => term -> (term, [(term, term)])
parseLamed (Lamed v b t) =
  let
    (b', o') = parseLamed b
   in
    (b', o' <> [(v, t)])
parseLamed x = (x, [])
parseCons :: (Term term) => term -> [term]
parseCons (Cons a0 a1) = (:) a0 $ parseCons a1
parseCons x = [x]
