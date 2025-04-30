module Rift.Core.Dev.Paeno where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Extra
import Rift.Core.Base
import Rift.Core.Dev.Parser
import Rift.Core.Unify.Dev
import Rift.Core.Unify.Unify

justAssume :: Maybe a -> a
justAssume v = case v of
  Just val -> val
  _ -> undefined

tRead :: String -> TestTerm
tRead = justAssume . readTerm
tReadL :: String -> [TestTerm]
tReadL = justAssume . readManyTerms

genTest :: [String] -> String -> (TestTerm, [TestTerm])
genTest [] str = (tRead str, [])
genTest (x : xs) str =
  let
    (term, vars) = genTest xs str
   in
    (term, (tRead x) : vars)

uniTest :: (Show (TestTerm)) => (TestTerm, [TestTerm]) -> (TestTerm, [TestTerm]) -> UnifyChoice (TestTerm)
uniTest (t0, v0) (t1, v1) =
  unifyTest ("Upper term" <?> t0) ("Lower term" <?> t1) ("Lowering vars" <?> v0) ("Raising vars" <?> v1)
t0 = tRead "(0 1 2)"
t1 = tRead "(0 1 (2 3))"

u0 = genTest ["2"] "(0 1 2)"
u1 = genTest [] "(0 1 (3 4))"

-- sys0 = tReadL "[(x *) {(x ~ x) *}]; [(x y *) {(y ~ x) (x ~ y)}]; [(x y *) {[(a b) {(<x a> ~ <y b>) (a ~ b)}] (x ~ y)}]; [(x y *) {[(a b) {(<a x> ~ <b y>) (a ~ b)}] (x ~ y)}];"
-- r0 = tRead "{((0 0) ~ (0 0)) *}"
-- r1 = tRead "{((0 1) ~ (0 1)) *}"
