module Sift.Search.Reduce where

import Control.Applicative (Alternative (..))
import Control.Monad.Morph
import Control.Monad.Reader
import Extra
import Extra.Choice
import Rift qualified
import Sift.Ops.Common
import Sift.Ops.Mem
import Sift.Ops.Zeta
import Sift.Search.Convert

reduce :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => (Rift.Inner t ~ t) => t -> OpM t
reduce t = (memReduce t <|> zetaReduce t)
reduceRec :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => (Rift.Inner t ~ t) => t -> OpM t
reduceRec t = (reduceRec' Nothing 0 t)
reduceRec' :: (Rift.Term t, Eq t, Show t, Rift.RTerm t) => (Rift.Inner t ~ t) => Maybe t -> Int -> t -> OpM t
reduceRec' (Nothing) i t = recover (pure t) $ do
  e <- ask
  reduced <- reduce t
  if (reduced == t) || (i > (e ^. opDepth)) then pure t else (reduceRec' Nothing (i + 1) reduced)
reduceRec' (Just g) i t = recover (pure t) $ do
  e <- ask
  reduced <- reduce t
  if (reduced == t) || (i > (e ^. opDepth)) then empty else cifte (cguard =<< convert reduced g) (pure t) (reduceRec' (Just g) (i + 1) reduced)
