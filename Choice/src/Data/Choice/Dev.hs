module Data.Choice.Dev (Alternative(..), module Data.Choice.Instances, pureChoice, Choice', pureChoice', runChoice') where 

import Control.Applicative
import Data.Choice.Core
import Data.Choice.Instances hiding (liftChoiceT)
import Data.Choice.Interface   
import Data.Choice.Types
import Data.Choice.Combinators
import Data.Choice.Pretty
import Data.Choice.Exports

type Choice' = ChoiceT IO
pureChoice :: Monad m => a -> ChoiceT m a 
pureChoice = pure

pureChoice' :: a -> Choice' a
pureChoice' = pure 

runChoice' :: Choice' a -> IO [a]
runChoice' = runChoiceT

