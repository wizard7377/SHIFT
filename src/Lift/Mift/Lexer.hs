{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Lift.Mift.Lexer where

import Data.Text qualified as T
import Lift.Core
import Lift.Mift.Common
import Text.Megaparsec

keyword :: T.Text -> PFMT t m ()
keyword k = try $ spacing $ do
  oneOf dollarSymbol
  chunk k
  pure ()

lexeme :: PFMT t m T.Text
lexeme = spacing $ do
  res <- some (noneOf specialSymbol)
  pure (T.pack res)
