module Rift.Dev.Paeno where

import Data.Text qualified as T
import Rift.Base
import Rift.Parser (readTerm)

justAssume :: Maybe a -> a
justAssume v = case v of
  Just val -> val
  _ -> undefined

tRead :: String -> TestTerm
tRead = justAssume . readTerm . T.pack

basenat = tRead "N"
t0 = tRead "{0 : N}"

-- it0 = introduce t0
tt0 = tRead "?<n>[{0 : N}]"

-- itt0 = introduce tt0
tt1 = tRead "?<n>[{n : N}]"

-- itt1 = introduce tt1
t1 = tRead "?<n>[{{(S n) : N} : {n : N}}]"
t2 = tRead "?<n>[?<m>[{(n = m) : ((S m) = (S n))}]]"
p0 = tRead "{(S 0) : N}"
m0 = tRead "{1 : N}"

t3 = tRead "?<n>[?<m>[ {{ {(n + m) : N} : {n : N} } : {m : N}} ]]"
