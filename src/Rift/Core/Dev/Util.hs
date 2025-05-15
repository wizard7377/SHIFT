module Rift.Core.Dev.Util where

import Control.Lens ((^.), _2)
import Data.Text qualified as T
import Extra
import Rift.Core.Base
import Rift.Core.Dev.Parser
import Rift.Core.Unify.Base
import Rift.Core.Unify.Dev
import Rift.Core.Unify.Unify
import System.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseTest)

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
genTest :: [String] -> String -> FTerm TestTerm
genTest [] str = ((tRead str), [])
genTest (x : xs) str =
  let
    (term, frees) = genTest xs str
   in
    (term, tRead x : frees)

selectAt :: [[a]] -> [Int] -> [a]
selectAt l = concatMap (l !!)
