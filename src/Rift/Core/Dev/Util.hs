module Rift.Core.Dev.Util where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Extra
import Rift.Core.Base
import Rift.Core.Dev.Parser
import Rift.Core.Ops.Mem hiding (FTerm)
import Rift.Core.Unify
import Rift.Core.Unify.Base
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseMaybe, parseTest)

justAssume :: Maybe a -> a
justAssume v = case v of
  Just val -> val
  _ -> undefined

tRead :: String -> TestTerm
tRead input = (justAssume . readTerm) input
tReadL :: String -> [TestTerm]
tReadL input = (justAssume . readManyTerms) input

tReadLL input = (justAssume . readManyTerms') input
readSys' :: FilePath -> [[TestTerm]]
readSys' path = tReadLL $ unsafePerformIO $ readFile path
readSys = readSys'
genTest :: [String] -> String -> Rift.Core.Unify.FTerm TestTerm
genTest [] str = (FTerm (tRead str) [])
genTest (x : xs) str =
  let
    FTerm term frees = genTest xs str
   in
    FTerm term (tRead x : frees)

selectAt :: [[a]] -> [Int] -> [a]
selectAt l = concatMap (l !!)

unifyTest :: (Term term, Ord term, Show term) => [term] -> [term] -> term -> term -> [UnifyState term]
unifyTest binds1 binds2 term1 term2 =
  unify (FTerm term1 binds1) (FTerm term2 binds2)

--  let
--    binds = generate term1 term2
--    env = initEnv binds1 binds2
--    result = (Prelude.flip unify) env <| binds
--   in
--    result

instance Read (TestTerm) where
  readsPrec _ str =
    case readTerm str of
      Just x -> [(x, "")]
      Nothing -> []
