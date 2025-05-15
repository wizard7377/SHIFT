{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Rift.Core.Unify.Base where

import Control.Applicative (Alternative (..))
import Control.Lens (makeLenses)
import Control.Lens.Operators
import Data.Functor.Identity (Identity)
import Data.List (intercalate)
import Extra hiding (empty)
import Extra.Basics
import Extra.Choice
import Extra.Map
import Rift.Core.Base
import Rift.Core.Instances

type BindingSet a = HMap a

{- | The unification enviroment and result
 - This not only encodes the list of variables, but also the existing bindings
 - Note that to avoid the confusing "lhs" and "rhs" terminology often used with unification, instead the terms "top" and "bottom" are used
 - This not only helps us with avoid the confusion, it also provides an intution, where the "botttom" tries to catch the "top", with appropiate "raising" and "lowering" motion
 -
-}
data UnificationEnv a = Unification
  { _varsUp :: [a]
  -- ^ The set of up vars
  , _varsDown :: [a]
  -- ^ The set of down vars
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
generate :: (Term term) => term -> term -> Choice (BindingSet term)
generate up down =
  "Generated"
    <?@> ( pure [(up, down)]
            <|> ( case (up, down) of
                    (Atom _, _) -> empty
                    (_, Atom _) -> empty
                    (Cons a0 a1, Cons b0 b1) -> (<>) <$> generate a0 b0 <*> generate a1 b1
                    (_, _) -> empty
                )
         )

initEnv :: (Term term) => [term] -> [term] -> UnificationEnv term
initEnv up down =
  Unification
    { _varsUp = up
    , _varsDown = down
    }

instance Semigroup (UnificationResult a) where
  (UnificationResult a0 b0 c0 d0) <> (UnificationResult a1 b1 c1 d1) = UnificationResult (a0 <> a1) (b0 <> b1) (c0 <> c1) (d0 <> d1)

instance Monoid (UnificationResult a) where
  mempty = UnificationResult mempty mempty mempty mempty

simpleResult = UnificationResult [] [] [] []
