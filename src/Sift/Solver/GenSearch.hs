{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeepSubsumption #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Sift.Solver.GenSearch where

-- TODO apart from generally making this faster, also probs should do something for parital init

import Control.Lens (over, _1, _2)
import Control.Monad (join)
import Control.Monad.Identity (Identity)
import Control.Monad.State
import Data.Foldable (Foldable (..))
import Data.List (intersect, singleton, subsequences)
import Data.Maybe (catMaybes, isJust, mapMaybe, maybeToList)
import Data.Traversable
import Debug.Trace (traceShow, traceShowId)
import Extra.Basics

-- import Extra.Choice (Choice (AllOf, AnyOf, EmptyNode, Simple, Trivial))

import Extra.Choice
import Extra.List (forEach, subParts)
import Extra.Map
import Extra.Tuple
import Rift
import Rift (Atomic)
import Sift (LogicResult (..))
import Sift.Base (LogicEnv, SAtom (..))
import Sift.Base qualified (LogicEnv (..))
import Sift.Monad ()
import Sift.Monad hiding (_depth)
import Sift.Monad qualified as Sift

-- import Sift.Types.Unify

type LMGen sen atom a = LM (SearchState sen atom) a

-- | The state of the Search monad with sentence `sen` on token `atom`
data SearchState sen atom = SearchState
  { _sentences :: [sen atom]
  -- ^ All proven statements
  , _depth :: Maybe Int
  -- ^ Current depth to end
  , _vars :: Int
  -- ^ List of infered vars
  }

instance EnterState SearchState Term where
  enterState :: LogicEnv -> [sen atom] -> SearchState sen atom
  enterState env sens =
    SearchState
      { _sentences = sens
      , _depth = Sift.Base._depth env
      , _vars = 0
      }

genSolve :: (Atomic atom) => LMGen sen atom (LogicResult ())
genSolve = _

-- TODO a lot of stuff

{- |
 - The general rule of transitivity, that is for two rules `a : b` and `b : c`, then `a : c`
 - This is complex as we do not just deal with concrete @Term@s, but also with terms that include @Lamed@s
 -
 - The general process goes as follows, for a given pair of terms `x` and `y`
 - 1. Introduce the Lameds in `x`, with frees in `xf` and root in `xt`. Do the same for `y` (into `yf` and `yt`)
 - 2. Try to match `xt` against `xt0 : xt1`, and `yt` against yt0 : yt1`, if this fails, then return `[]`
 - 3. Introduce `xt1` into `xtt` and `xtf` and `yt0` into `ytt` and `ytf`
 - 4. Create a unification of `ytt ~> xtt`, and call it `uni`
 - 5. Split the given choices of the unification as to whether they are in `ytf` or `xtf` (with, for instance "free to not free" denotation free in `ytf` and not free in `xtf`), into `(uni0,uni1)`
 -  * All images from not free to not free are passed through on the left (NOT DONE YET)
 -  * The map from free to free must be surjective. This is not passed through
 -  * The map from free to not free must be surjective, in addition this may not share any of keys with the "free to free" map. This is passed through on the right
 -  * The map from not free to free must be empty, and it is (trivially) not passed through
 - 6. Attempt to solve `uni0`, without left overs, with left free `yf` and right free `xt`, if this fails, the return `[]`
 - 7. If this dosen't fail, then use use the given bindings in `xt0` and `yt1`, as `x'` and `y'`
 - 8. Return `x' : y'`
 -
-}
memFill :: (Atomic atom) => Term atom -> Term atom -> LMGen sen atom [Term atom]
memFill termA termB = do
  let termA1 = "termA1" <?> intros termA
  let termB1 = "termB1" <?> intros termB
  return $
    case (termA1 ^. root, termB1 ^. root) of
      (Rule gA pA, Rule gB pB) ->
        let pA1 = "pA" <?> intros pA
            gB1 = "gB" <?> intros gB
            -- These bindings end up being backwards
            uni = "uni" <?> unify (gB1 ^. root) (pA1 ^. root)
            possible =
              cfilterMap
                ( \choice ->
                    let
                      -- (Not free to not free, free to not free, not free to free, free to free)
                      (basic, freeing, incomplete, binding) = split4 (\e -> fst e `elem` gB1 ^. free) (\e -> snd e `elem` pA1 ^. free) choice
                      -- If anything constant has tried to become free
                      goodAchieve = null incomplete
                      isOnto = msuject binding
                      -- A variable does not both bind to a free and a not free
                      isSingle = intersect (getKeys freeing) (getKeys binding) == []
                     in
                      if goodAchieve && isSingle && isOnto then Just basic else Nothing
                )
                uni
            vtree = "vtree" <?> uTree possible (termB1 ^. free) (termA1 ^. free)
            stree = "stree" <?> Rift.solve vtree
            -- prepReq = cfilterMap $ \(binds, state) -> if null state then Just binds else Nothing
            prepReq = cfilterMap $ \(binds, state) -> if True then Just binds else Nothing
            prepReqV = prepReq stree
            -- TODO
            prepMapA = (mapToF . (view bindingLeft)) <$> prepReqV
            prepMapB = (mapToFR . (view bindingRight)) <$> prepReqV
            -- TODO
            prepA = "prepA" <?> (prepMapA <*> pure gA)
            prepB = "prepB" <?> (prepMapB <*> pure pB)
            newTerm = Rule <$> prepA <*> prepB
         in resolve newTerm
      _ -> []

-- | ?f(x : y) -> (?f(x) : ?f(y))
lamedSplit :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
lamedSplit = lamedSplit' . intros

lamedSplit' :: (Atomic atom) => QTerm atom -> LMGen sen atom [Term atom]
lamedSplit' term = return $ case term ^. root of
  (Rule to from) -> [Rule (addTo to (term ^. free)) (addTo from (term ^. free))]
  _ -> []

-- | x -> (x : *)
yudGen :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudGen term = do
  let qterm = intros term
  let parts = partApply (\conq -> singleton $ Rule (unintros conq) Yud) $ mkQTerm (qterm ^. root) (qterm ^. free)
  return $ unintros <$> parts

-- | (x : *) -> x
yudRed :: (Atomic atom) => Term atom -> LMGen sen atom [Term atom]
yudRed = yudRed' . intros

yudRed' :: (Atomic atom) => QTerm atom -> LMGen sen atom [Term atom]
yudRed' term = return $ case term ^. root of
  Rule val Yud -> [unintros $ mkQTerm val (term ^. free)]
  _ -> []
yudGen' :: (Atomic atom) => QTerm atom -> Term atom
yudGen' term = unintros $ mkQTerm (Rule (term ^. root) Yud) (term ^. free)
