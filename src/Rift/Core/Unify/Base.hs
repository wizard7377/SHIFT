{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rift.Core.Unify.Base where

import Control.Applicative (Alternative (..))
import Control.Lens (makeLenses)
import Control.Lens.Operators
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Extra
import Extra.Basics
import Extra.Choice
import Extra.Map
import Rift.Core.Base
import Rift.Core.Instances

{- | A simple alias
Note that to avoid the confusing "left" and "right" terms commenly used, the terms top (general or first) and bottom (specific or second)
That is, it is of the form `(Top, Bottom)`.
A "lowering" refers to the operation from `x` to `y`, with respect to `(x,y)`, as it goes from top to bottom A raising refers to `y` to `x`
-}
data FTerm a = FTerm {_term :: Term' a, _frees :: [Term' a]}

deriving instance (Eq a) => Eq (FTerm a)

showFTerm :: (Show a) => FTerm a -> [Char]
showFTerm (FTerm term frees) = concatMap (\x -> "∀" ++ show x) frees ++ " . " ++ show term

-- showFListT v = if null v then ("{}") else "\n[\n\t" ++ (intercalate "\n\t" $ (show <$> v)) ++ "\n]"
instance (Show atom) => Show (FTerm atom) where
  show = showFTerm

-- showList x = (++ showFListT x)
simpleF :: Term' atom -> FTerm atom
simpleF t = FTerm t []
addFrees :: (Atomic atom) => FTerm atom -> [Term' atom] -> FTerm atom
addFrees (FTerm t fs) new = FTerm t (fs <> new)
replaceTerm :: (Atomic atom) => FTerm atom -> Term' atom -> FTerm atom
replaceTerm (FTerm _ fs) new = FTerm new fs
type BindingSet a = HMap a

data UnificationEnv a = Unification
  { _binds :: BindingSet a
  , _varsUp :: [a]
  , _varsDown :: [a]
  }
  deriving (Show, Eq)

data UnificationResult a = UnificationResult
  { _lowering :: BindingSet a
  , _raising :: BindingSet a
  , _upBinds :: [a]
  , _downBinds :: [a]
  }
  deriving (Show, Eq)

makeLenses ''UnificationEnv
makeLenses ''UnificationResult

type UnificationAttempt a = Choice (UnificationResult a)
generate :: (Atomic atom, Show (Term (BasicAtom atom))) => Term' atom -> Term' atom -> Choice (BindingSet (Term' atom))
generate up down =
  "Generated"
    <?@> ( pure [(up, down)]
            <|> ( case (up, down) of
                    (Atom _, _) -> empty
                    (_, Atom _) -> empty
                    (Cons a0 a1, Cons b0 b1) -> (<>) <$> (generate a0 b0) <*> (generate a1 b1)
                    (_, _) -> empty
                )
         )

initEnv :: (Atomic atom) => BindingSet atom -> [atom] -> [atom] -> UnificationEnv atom
initEnv binds up down =
  Unification
    { _binds = binds
    , _varsUp = up
    , _varsDown = down
    }

instance Semigroup (UnificationResult a) where
  (UnificationResult a0 b0 c0 d0) <> (UnificationResult a1 b1 c1 d1) = UnificationResult (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1)

instance Monoid (UnificationResult a) where
  mempty = UnificationResult mempty mempty mempty mempty
