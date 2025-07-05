module Sift.Search.Reduce where

import Control.Applicative (Alternative (..))
import Control.Monad.Morph
import Control.Monad.Reader
import Extra
import Extra.Choice
import Rift qualified
import Sift.Ops.Common
import Sift.Ops.Fix
import Sift.Ops.Mem
import Sift.Ops.Simple
import Sift.Ops.Zeta
import Sift.Search.Convert

reduce :: (Rift.Term (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> OpM e
reduce t = (memReduce t <|> zetaReduce t <|> deltaReduce t <|> fixReduce t)
reduceRec :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> OpM e
reduceRec t = do
  env <- ask
  (reduceRec' (env ^. opGoal) 0 t)
reduceRec' :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => Maybe (Rift.TermOf e) -> Int -> (Rift.TermOf e) -> OpM e
reduceRec' (Nothing) i t = recover (pure t) $ do
  e <- ask
  reduced <- reduce t
  if (reduced == t) || (i > (e ^. logicEnv ^. Rift.logicEnvDepth)) then pure t else (reduceRec' Nothing (i + 1) reduced)
reduceRec' (Just g) i t = recover (pure t) $ do
  e <- ask
  reduced <- reduce t
  if (i > (e ^. logicEnv ^. Rift.logicEnvDepth)) then empty else cifte (convert [] reduced g) (pure t) (reduceRec' (Just g) (i + 1) reduced)

searchTest :: (Rift.Theory e, Rift.Term t, Eq t, Rift.TermOf e ~ t, Show t, Rift.RTerm t) => e -> t -> t -> [t]
searchTest e t0 t1 =
  let
    env = OpEnv{_logicEnv = def, _opTheory = e, _opGoal = Just t1}
   in
    runOpM (reduceRec t0) env
