module Rift.Core.Dev.Paeno where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Rift.Core.Base
import Rift.Core.Parser (readTerm)
import Rift.Core.Unify.Unify

justAssume :: Maybe a -> a
justAssume v = case v of
  Just val -> val
  _ -> undefined

tRead :: String -> TestTerm
tRead = justAssume . readTerm . T.pack

basenat = tRead "N"
t0 = tRead "{0 : N}"

-- it0 = introduce t0
tt0 = tRead "?<n>{0 : N}"

-- itt0 = introduce tt0
tt1 = tRead "?<n>{n : N}"

-- itt1 = introduce tt1
t1 = tRead "?<n>{{(S n) : N} : {n : N}}"
t2 = tRead "?<n>?<m>{(n = m) : ((S m) = (S n))}"
p0 = tRead "{(S 0) : N}"
m0 = tRead "{1 : N}"

t3 = tRead "?<n>?<m> {{ {(n + m) : N} : {n : N} } : {m : N}}"

lax0 = tRead "{T : *}"
lax1 = tRead "?<A B>{{(A & B) : B} : A}"
pax0 = tRead "{0 : N}"
pax01 = tRead "?<n>{0 : N}"
pax02 = tRead "?<n>{n : N}"
pax1 = tRead "?<n>{{(s n) : N} : {n : N}}"
pax2 = tRead "?<n>{(gt n 0) : {n : N}}"
pax3 = tRead "?<m n>{((s m) = (s n)) : (n = m)}"
it0 = intros t0
itt0 = intros tt0

treet0 = uTree (unify (fst it0) (fst itt0)) (it0 ^. _2) (itt0 ^. _2)
