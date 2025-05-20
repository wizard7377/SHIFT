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

leftParen = choice [keyword "("]
leftBrace = choice [keyword "{"]
leftBracket = choice [keyword "["]
rightBracket = choice [keyword "]"]
rightBrace = choice [keyword "}"]
rightParen = choice [keyword ")"]
kaxiom = choice [keyword "a"]
kcons = choice [keyword "c", keyword "."]
kdefine = choice [keyword "d"]
kexpect = choice [keyword "e"]
kimport = choice [keyword "i"]
klamed = choice [keyword "l", keyword "?"]
kproof = choice [keyword "p", keyword "p"]
kref = choice [keyword "r", keyword "&"]
ksymbol = choice [keyword "m"]
