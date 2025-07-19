module Data.Choice (
  -- * The core choice monad

  --
  -- $choice

  -- ** Instances
  -- $inst
  module Data.Choice.Exports,
) where

-- \$choice
-- The choice monad, conceptually, provides a way to have non-deterministic computations with shared state.
--
-- It is often useful to think of the choice monad as a way to view one possible state at a time, with access to a shared monad.
-- For instance, the type `ChoiceT IO (a,b)`, might represent a function that takes two lists, takes their cross product, and then divides the two creating an IO exception on error.
-- The monadic bind operator, then, chooses one path to go on
--
-- It is also helpful to view 'ChoiceT' as a way to engage in parsing, as the two tasks are isomorphic.
-- That is, we use certain "parser" combinators as "logical" combinators
-- For instance, @&@ (lookahead) corresponds to split and join '&&&'.
-- @|@ (alternation), corresponds to alternatives `<|>`
-- @,@ (sequencing), corresponds to either `>>=` or `>>` (depending on if the proof of the first is relevant)

-- \$inst
-- The `Choice` monad is an instance of quite many types
-- Firstly, it is a
import Data.Choice.Exports
