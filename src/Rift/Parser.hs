{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Rift.Parser (readTerm) where

import Text.Megaparsec
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParsecT, anySingle, choice, many, noneOf, oneOf, parseTest, single, some)
import Text.Megaparsec.Char.Lexer (lexeme)

import Control.Monad (void)
import Data.Functor (($>))
import Data.Functor.Identity
import Data.List (singleton)
import Data.Text qualified as T
import Data.Void
import Rift.Base
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type ParserG a = ParsecT Void T.Text Identity a
type Parser = ParsecT Void T.Text Identity TestTerm
type Lexer = ParsecT Void T.Text Identity T.Text
numbers :: [Char]
numbers = ['0' .. '9']
letters :: [Char]
letters = ['a' .. 'z'] ++ ['A' .. 'Z']
specialChars :: [Char]
specialChars = "():?*!{}<>[]"
otherChar :: Char -> Bool
otherChar v = not (elem v numbers || elem v letters || elem v specialChars || v == ' ')

glyphP :: Parser
glyphP = do
  res <-
    spaced
      ( some (oneOf (letters ++ numbers))
          <|> some (satisfy otherChar)
      )
  return $ Atom $ T.pack res

-- leftParen :: Lexer
leftParen = spaced $ string "("
rightParen = spaced $ string ")"
leftBracket = spaced $ string "["
rightBracket = spaced $ string "]"
leftAngle = spaced $ string "<"
rightAngle = spaced $ string ">"
leftBrace = spaced $ string "{"
rightBrace = spaced $ string "}"
parens = between leftParen rightParen
colon = spaced $ string ":"
lyud = spaced $ string "*"
llamed = spaced $ string "?"

plamed = try $ do
  llamed
  leftAngle
  bound <- pterm
  rightAngle
  leftBracket
  term <- pterm
  rightBracket
  return $ Lamed bound term
prule = try $ do
  leftBrace
  t0 <- pterm
  colon
  t1 <- pterm
  rightBrace
  return $ Rule t0 t1
plist :: Parser
plist = try $ do
  leftParen
  l <- some pterm
  rightParen
  return $ foldr Cons Empty l
pyud = try $ Yud <$ lyud
spaced :: ParserG a -> ParserG a
-- spaced = lexeme space1
spaced parser = do
  space
  res <- parser
  -- space ;
  return res
pterm :: Parser
pterm =
  choice
    [ prule
    , plist
    , plamed
    , pyud
    , glyphP
    ]

-- | Note that lists are `()`, rules `{}`, and lameds `<>[]`
readTerm :: T.Text -> Maybe (TestTerm)
readTerm = parseMaybe pterm
