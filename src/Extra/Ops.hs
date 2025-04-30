{-# LANGUAGE ExplicitNamespaces #-}

module Extra.Ops (
  (<$$>),
  (<*+>),
  (</>),
  (-?>),
  (|>),
  (<|),
  (%%),
  type (->>),
  (%~),
  (^.),
  (.~),
)
where

import Control.Arrow qualified as Arrow
import Control.Lens.Operators hiding ((<|), (|>))

infixr 1 -?>
(-?>) = implies

infixl 4 <$$>

-- | Nested `<$>`
(<$$>) :: (Functor f0) => (Functor f1) => (a -> b) -> f1 (f0 a) -> f1 (f0 b)
(<$$>) = fmap . fmap

(<*+>) = (<*>) . (<*>)
infixl 4 </>
(</>) :: (Monad m) => (a -> m b) -> m a -> m b
(</>) f a = a >>= f

-- | A synoynm for `>>=`
(|>) :: (Monad m) => m a -> (a -> m b) -> m b
(|>) = (>>=)

-- | A synoynm for `=<<`
(<|) :: (Monad m) => (a -> m b) -> m a -> m b
(<|) = flip (|>)

infixl 2 |>
infixr 2 <|

-- | The simple boolean implication function, that is \(a \to b\)
implies :: Bool -> Bool -> Bool
implies a b = a || not b

-- | The boolean exclusive or function
xor :: Bool -> Bool -> Bool
xor a b = (a || b) && not (a && b)

(%%) = xor
infixl 7 %%
type a ->> b = forall arr. (Arrow.Arrow arr) => arr a b
