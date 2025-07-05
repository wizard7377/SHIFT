{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Tift.Lexer
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Tift.Lexer where

-- Plan for lexing is as follows:
-- Each token can be:
-- A series of latin script or arabic numerals, including diacratics or
-- Exactly one symbol from a non-latin alphabet or
-- A sequence of symbols not otherwised mentioned

import Control.Lens.Operators ((+=))
import Control.Monad.RWS (get)
import Data.Text qualified as T
import Extra
import Lift.Tift.Expr
import Lift.Tift.Types
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char.Lexer qualified as L

sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc
keyword :: [Parser a] -> Parser p -> Parser [p]
keyword key p = lexeme ((try $ "@" >> choice key) >> plist p)
special :: [Parser a] -> Parser ()
special key = lexeme ((try $ "@" >> choice key) >> pure ())
plist :: Parser p -> Parser [p]
plist p = (try "[" >> some (try p <* sc) <* "]") <|> (pure [])
index :: Parser Int
index = lexeme (try $ "@" >> (some C.numberChar <&> read))
maybeIndex :: Parser Int
maybeIndex = index <|> pure 1
normal = noneOf (" \n\t()@[]{}._" :: String)
atomicP :: Parser T.Text
atomicP = lexeme $ do
  tok <-
    choice
      [ try (some (lookAhead latin1Char >> lookAhead alphaNumChar >> normal))
      , try (some (lookAhead symbolChar >> normal))
      , pure <$> (lookAhead alphaNumChar >> normal)
      ]
  pure $ T.pack tok

maybeIndexed :: Parser a -> Parser Int
maybeIndexed tok =
  (try $ head <$> keyword [tok] decimal)
    <|> ( do
            keyword [tok] (fail "")
            state <- get
            cindex += 1
            pure $ state ^. cindex
        )
