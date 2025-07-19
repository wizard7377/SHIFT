{-# LANGUAGE PartialTypeSignatures #-}

module Tests.Convert where

import Control.Exception (throw)
import Control.Lens.Operators ((&))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Default
import Extra
import Extra.Color
import Extra.Parsers
import Extra.TestHelp (cleanUp)
import GHC.IO (throwIO)
import Rift (TestTerm)
import Rift qualified
import Short
import Sift.Core.Dev
import Sift.Core.Monad
import Sift.Core.Types
import Sift.Search
import Sift.Search.Reduce
import Test.HUnit
import Text.Printf (printf)

-- | A version of `solve`, when it needs to be monomorphic.
solveMono :: Rift.LogicEnv -> TestTheory -> TestTerm -> TestTerm -> LogicResult TestTheory
solveMono = solve

type TestTheory = Rift.SimpleTheory () Rift.TestToken Rift.TestTerm
convertTestBy :: Bool -> TestTheory -> String -> String -> Assertion
convertTestBy expect thy s0 s1 = do
  x <- tRead ("x is: " ?> s0)
  y <- tRead ("y is: " ?> s1)
  let (LogicResult globe paths res) = solve def thy x y
  assertBool (show $ showProblem expect x y (LogicResult globe paths res)) (res == expect)
convertTestBy' :: Bool -> TestTheory -> String -> String -> IO _
convertTestBy' expect thy s0 s1 = do
  x <- tRead ("x is: " ?> s0)
  y <- tRead ("y is: " ?> s1)
  case solve def thy x y of
    r@(LogicResult globe paths res) -> do
      if res == expect
        then print "Good" >> pure r
        else print "Bad" >> pure r

showProblem :: (TOC e, Show e) => Bool -> TO e -> TO e -> LogicResult e -> String
showProblem goal t0 t1 (LogicResult globe paths res) =
  show $
    ( (if goal then "Should have worked:" else "Shoudl have failed:")
        <> unlines
          [ "Convert: " <> show t0 <> " to " <> show t1
          , "Global: " <> show globe
          , "Choices: " <> show paths
          ]
    )

convertTest = convertTestBy True
convertFailTest = convertTestBy False
parseTest :: (Monad (MonadOf TestTerm), Parsable TestTerm) => String -> (MonadOf TestTerm) (Rift.TestTerm)
parseTest x = preadWithIO x
