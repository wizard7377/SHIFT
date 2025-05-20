module Rift.Core.Ops.Mem where

import Control.Lens (makeClassy)
import Control.Lens.Lens
import Data.List ((\\))
import Extra
import Rift.Core.Base qualified as Rift
import Rift.Core.Kernel qualified as Rift
import Rift.Core.Unify.Base qualified as Rift
import Rift.Core.Unify.Unify qualified as Rift

data FTerm term = FTerm
  { _term :: term
  , _frees :: [term]
  }
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

toFTerm :: (a, [a]) -> FTerm a
toFTerm (t, fs) = FTerm t fs
fromFTerm :: FTerm a -> (a, [a])
fromFTerm (FTerm t fs) = (t, fs)
mem :: (Rift.Term term, Eq term, Ord term, Show term) => FTerm term -> FTerm term -> [FTerm term]
mem top@(FTerm (Rift.Lamed var upFrom upTo) freeUp) bottom@(FTerm down freeDown) = mem' upFrom upTo (var : freeUp) down freeDown
mem' :: (Rift.Term term, Rift.TermLike term) => term -> term -> [term] -> term -> [term] -> [FTerm term]
mem' upFrom upTo freeUp down freeDown =
  let
    uni = Rift.generate upFrom down
    vUp = freeUp
    vDown = freeDown
    env = (pure Rift.initEnv) <*> pure freeUp <*> pure freeDown
    ur = concat $ Rift.unify <$> uni <*> env
    res =
      ( \ures ->
          let
            mapping = mapToF $ ures ^. Rift.lowering
            newvars = vUp \\ ures ^. Rift.upBinds
           in
            FTerm (Rift.recurseSome mapping upTo) newvars
      )
        <$> ur
   in
    res
