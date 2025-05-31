module Rift.Core.Ops.Mem (
    {-# DEPRECATED "Use memReduce instead" #-}
  mem,
    {-# DEPRECATED "Use memReduce instead" #-}
  mem',
  memReduce,
) where

import Control.Lens (makeClassy)
import Control.Lens.Lens
import Data.List ((\\))
import Extra
import Rift.Core.Base
import Rift.Core.Base qualified as Rift
import Rift.Core.Interface (FTerm (..))
import Rift.Core.Interface qualified as Rift
import Rift.Core.Kernel qualified as Rift
import Rift.Core.Unify qualified as Rift
import Rift.Core.Unify.Base qualified as Rift
import Rift.Core.Unify.Conclude (conclude)

toFTerm :: (a, [a]) -> FTerm a
toFTerm (t, fs) = FTerm t fs
fromFTerm :: FTerm a -> (a, [a])
fromFTerm (Rift.FTerm t fs) = (t, fs)
{-# SCC mem #-}
mem :: (Rift.Term term, Eq term, Ord term, Show term, Rift.FTermLike term, Rift.UTermLike term gen) => gen -> FTerm term -> FTerm term -> Choice (FTerm term)
mem gen top@(Rift.FTerm (Rift.Lamed var upFrom upTo) freeUp) bottom@(Rift.FTerm down freeDown) = mem' gen upFrom upTo (var : freeUp) down freeDown
{-# DEPRECATED mem, mem' "Use memReduce instead" #-}
{-# SCC mem' #-}
mem' :: (Rift.Term term, Rift.TermLike term, Rift.FTermLike term, Rift.UTermLike term gen) => gen -> term -> term -> [term] -> term -> [term] -> Choice (FTerm term)
mem' gen upFrom upTo freeUp down freeDown = memReduce gen freeUp upFrom upTo (FTerm down freeDown)

{-# SCC memReduce #-}
{-# INLINE memReduce #-}

{- | The core Mem ם rule
 - Roughly equivalent to λ-calculus' β reduction
 - That is, the statement ([x]{y.z} t) is roughly equivalent to (∀ה ((λx.λy.z) t ה))
 - ם reduction, then
-}
memReduce ::
  forall term arg gen.
  (Rift.Term term, Rift.TermLike term, Rift.FTermLike arg, Rift.Inner arg ~ term, Rift.FTermLike term, Rift.UTermLike term gen) =>
  gen ->
  -- | The list of variables in the ל term
  [term] ->
  -- | The input of the ל template
  term ->
  -- | The output of the ל template
  term ->
  -- | The term, with frees, to apply
  arg ->
  -- | The list of all possible reductions
  Choice (FTerm term)
memReduce gen freeUp upFrom upTo down =
  let
    ur = "Ur" <?> Rift.unify (upFrom, freeUp) down
    ur' = "Ur'" <?> conclude gen ur
    res =
      ( \(ures) ->
          let
            mapping = Rift.mapUp ures
            mappingB = Rift.mapDown ures
            newvars = Rift.getFree (ures ^. Rift.upState) <> (Rift.getFree (ures ^. Rift.downState))
           in
            FTerm (Rift.recurseManyA (mapping) mappingB upTo) newvars
      )
        <$> ur'
   in
    res
