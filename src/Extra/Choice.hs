module Extra.Choice where
import Control.Applicative (Alternative, (<**>), liftA)
import Data.Maybe (catMaybes, mapMaybe, isJust, isNothing)
import Debug.Trace
import Data.List (singleton)
import Extra.Decide (Decide (..))

--data 
data Choice a =
    Trivial
    | EmptyNode
    | Simple a
    | AllOf [Choice a]
    | AnyOf [Choice a]
    deriving (Show,Functor,Eq)


instance Semigroup (Choice a) where
  (<>) :: Choice a -> Choice a -> Choice a
  a <> b = AllOf [a,b]
  {-
  -- | F & _ = F
  EmptyNode <> _ = EmptyNode
  -- | _ & F = F
  _ <> EmptyNode = EmptyNode
  AllOf l0 <> AllOf l1 = AllOf (l0 <> l1)
  AnyOf l0 <> v = AnyOf (fmap (<> v) l0) --TODO
  v <> AnyOf l0 = AnyOf (fmap (<> v) l0)
  Trivial <> v = v
  v <> Trivial = v
  Simple a <> AllOf b = AllOf (Simple a : b)
  AllOf a <> Simple b = AllOf (Simple b : a)
  Simple a <> Simple b = AllOf [Simple a, Simple b]
  -}
instance Monoid (Choice a) where
  mempty :: Choice a
  mempty = Trivial

instance Applicative Choice where
  pure :: a -> Choice a
  pure = Simple

  -- |Sequence some pair of possibilites, that is, for instance `A & B` 
  (<*>) :: Choice (a -> b) -> Choice a -> Choice b
  t0 <*> t1 = case t0 of
    Trivial -> Trivial
    EmptyNode -> EmptyNode
    Simple f -> f <$> t1
    {-
      Ok this is a little complex so lets go through it
      ((a -> b) & (a->c)) d > b(d) & c(d)
    -}
    AllOf lf -> AllOf $ liftA2 (<*>) lf (pure t1)
    AnyOf lf -> AnyOf $ liftA2 (<*>) lf (pure t1)
    _ -> EmptyNode


{-
notnull = not . null
decideWithAcc :: (Show a, Monoid b) => (b -> a -> [b]) -> b -> Choice a -> Bool
decideWithAcc eval init tree = not . null $ decideWithAcc' eval init tree
-- | Take in a
decideWithAcc' :: (Show a, Monoid b) => (b -> a -> [b]) -> b -> Choice a -> [b]
decideWithAcc' eval init tree =
  case tree of {
    Trivial -> singleton init ;
    EmptyNode -> [] ;
    AllOf (x:xs) -> let r0 = myeval init x in myeval r0 (AllOf xs)  ;
    AnyOf l -> let res = decideWithAcc' eval init <$> l in if (any isNothing res) then Nothing else mconcat res ;
    Simple v -> eval init v ;
  } where myeval = decideWithAcc' eval
-}

-- Theres probaly a cool thing to do here idk
-- |Attempt to solve a decision tree

{-
attemptSolve :: (Monoid b, Alternative b) => (a -> b -> b) -> Choice a -> b -> b
attemptSolve eval tree init = case tree of {
    Trivial -> b ;
    EmptyNode -> empty ;
    AllOf [x] -> attemptSolve eval x init ;
    AllOf (x:xs) -> let r0 = attemptSolve eval x init in  _ 
  }

-}