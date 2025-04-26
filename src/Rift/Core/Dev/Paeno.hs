module Rift.Core.Dev.Paeno where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Rift.Core.Base
import Rift.Core.Parser
import Rift.Core.Parser (readTerm)
import Rift.Core.Unify.Unify

justAssume :: Maybe a -> a
justAssume v = case v of
  Just val -> val
  _ -> undefined

tRead :: String -> TestTerm
tRead = justAssume . readTerm . T.pack
tReadL :: String -> [TestTerm]
tReadL = justAssume . readManyTerms . T.pack
sys0 = tReadL "[(x *) {(x ~ x) *}]; [(x y *) {(y ~ x) (x ~ y)}]; [(x y *) {[(a b) {(<x a> ~ <y b>) (a ~ b)}] (x ~ y)}]; [(x y *) {[(a b) {(<a x> ~ <b y>) (a ~ b)}] (x ~ y)}];"
r0 = tRead "{((0 0) ~ (0 0)) *}"
r1 = tRead "{((0 1) ~ (0 1)) *}"
