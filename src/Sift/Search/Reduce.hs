module Sift.Search.Reduce where

import Control.Applicative (Alternative (..))
import Control.Monad.Morph
import Control.Monad.RWS qualified as M
import Control.Monad.Reader
import Extra
import Extra.Choice
import Rift qualified
import Sift.Core.Types
import Sift.Ops.Common
import Sift.Ops.Fix
import Sift.Ops.Mem
import Sift.Ops.Simple
import Sift.Ops.Zeta
import Sift.Search.Convert

reduce :: (Rift.Term (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> LogicM e (Rift.TermOf e) (Rift.TermOf e)
reduce t = (memReduce t <|> zetaReduce t <|> deltaReduce t <|> fixReduce t)

reduceRec :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => (Rift.TermOf e) -> LogicM e (Rift.TermOf e) (Rift.TermOf e)
reduceRec t = do
  env <- M.get
  (reduceRec' 0 $ Just t)

convertRec :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => LogicM e (Rift.TermOf e) (Rift.TermOf e)
convertRec = do
  env <- M.get
  (reduceRec' 0 Nothing)
reduceRec' :: (Rift.Term (Rift.TermOf e), Eq (Rift.TermOf e), Show (Rift.TermOf e), Rift.RTerm (Rift.TermOf e)) => (Rift.Inner (Rift.TermOf e) ~ (Rift.TermOf e)) => Int -> Maybe (Rift.TermOf e) -> LogicM e (Rift.TermOf e) (Rift.TermOf e)
reduceRec' i (Just t) = recover (pure t) $ do
  e <- ask
  s <- M.get
  reduced <- reduce (s ^. curTerm)
  curTerm .= reduced
  if (i > (e ^. logicEnv ^. Rift.logicEnvDepth)) then empty else cifte (convert [] reduced t) (pure reduced) (reduceRec' (i + 1) (Just t))
reduceRec' i Nothing = do
  e <- ask
  s <- M.get
  reduced <- reduce (s ^. curTerm)
  curTerm .= reduced
  if (i > (e ^. logicEnv ^. Rift.logicEnvDepth)) then empty else (reduceRec' (i + 1) Nothing)
searchTest :: (Rift.Theory e, Rift.Term t, Eq t, Rift.TermOf e ~ t, Show t, Rift.RTerm t) => e -> t -> t -> [t]
searchTest e t0 t1 =
  let
    env = OpEnv{_logicEnv = def, _opTheory = e}
   in
    runOpM (reduceRec t0) env
