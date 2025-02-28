module Sift.Solver.BasicSearch where

import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))
import Rift
import Sift.Base (LogicEnv)
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- import Rift.Funcs (intros)
import Data.Tree

{-
-- |The state of the Search monad with sentence `sen` on token `tok`
data SearchState sen tok = SearchState {
    -- |All proven statements
    _sentences :: [sen tok],
    -- |Current depth to end
    _depth :: Maybe Int,
    -- |List of infered vars
    _vars :: Int
}
{-
instance Sentence sen tok => EnterState (SearchState sen tok) where
  --enterState :: LogicEnv -> [sen tok] -> SearchState sen tok
  enterState env sens =  SearchState {
    _sentences = sens,
    _depth = Sift.Types._depth env,
    _vars = 0
  }
-}
type PosSearch a = Choice (Term a)
type LMS tok a = forall sen. Sentence sen tok  => LM (SearchState sen tok) a
type SearchAction a = Token a => Term a -> LMS a (PosSearch a)
yudGen :: SearchAction a
yudRed :: SearchAction a
yudGen val = return $ Simple $ Rule val Yud --TODO
yudRed (Rule val Yud) = return $ Simple val
yudRed _ = return EmptyNode
transSolveBasic :: Term a -> Term a -> LMS a (PosSearch a)
transSolveBasic iterm0 iterm1 =
  let
    (term0,bounds0) = intros term0
    (term1,bounds1) = intros term1
  in case (term0,term1) of {
    ((Rule to0 from0),(Rule to1 from1)) -> _ ;
    (_,(Rule to0 from0)) -> _
  }
-}
