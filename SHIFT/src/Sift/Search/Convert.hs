module Sift.Search.Convert where

import Control.Applicative (Alternative (..))
import Data.Functor (($>))
import Extra
import Rift qualified
import Sift.Core.Unify
import Sift.Ops.Common
import Sift.Ops.Simple

convert :: (Rift.Theory thy) => Unify thy (Rift.TermOf thy)
convert frees t0 t1 =
  ( nullConvert t0 t1
      $> emptyUni frees
  )
    <|> ( do
            uni <- unifyConvert frees t0 t1
            pure uni
        )

convert' :: (Rift.Theory thy) => [Rift.TermOf thy] -> Convert thy (Rift.TermOf thy)
convert' frees t0 t1 = applyRes <$> (convert frees t0 t1) <*> pure t0
