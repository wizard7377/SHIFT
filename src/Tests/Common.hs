module Tests.Common where

import Control.Lens (universe)
import Control.Lens.Plated (Plated)
import Extra.Parsers
import Rift qualified
import Sift qualified
import Test.QuickCheck qualified as QC

termRead :: String -> IO (Rift.TestTerm)
termRead = preadWithIO

genOccurs :: (Plated Rift.TestTerm) => Rift.TestTerm -> QC.Gen Rift.TestTerm
genOccurs term = do
  let termsIn = universe term
  QC.elements termsIn
