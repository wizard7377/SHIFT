{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

{- | A basic debug parser for Rift
 - Note that this is /not/ a front end user facing thing, rather, this is just for quick and dirty debugging
-}
module Rift.Core.Parser (readTerm, readTermTest, readManyTerms, readTermsTest) where

import Text.Megaparsec
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParsecT, anySingle, choice, many, noneOf, oneOf, parseTest, single, some)
import Text.Megaparsec.Char.Lexer (lexeme)

import Control.Monad (void)
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.Functor.Identity
import Data.List (singleton)
import Data.Text qualified as T
import Data.Void
import Extra.List
import Rift.Core.Base
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

type ParserG a = ParsecT Void T.Text Identity a
type Parser = ParsecT Void T.Text Identity TestTerm
type Lexer a = ParsecT Void T.Text Identity (BasicAtom a)
numbers :: [Char]
numbers = ['0' .. '9']
letters :: [Char]
letters = ['a' .. 'z'] ++ ['A' .. 'Z']
specialChars :: [Char]
specialChars = "()!{}<>[]%#; "
otherChar :: Char -> Bool
otherChar v = not (elem v numbers || elem v letters || elem v specialChars || v == ' ')

{-
glyphP :: Parser
glyphP = do
  res <-
    spaced
      ( some (oneOf (letters ++ numbers))
          <|> some (satisfy otherChar)
      )
  return $ AAtom $ T.pack res
-}
lexCore :: BasicAtom atom -> T.Text -> Lexer atom
lexCore atom str = try $ do
  void $ spaced $ (string "%" >> string str)
  return atom
lexT :: Lexer T.Text
lexT =
  try $
    spaced $
      choice
        [ lexCore ALamed "?"
        , lexCore ARule ":"
        , (AAtom <$> T.pack <$> (some (noneOf specialChars)))
        ]

-- leftParen :: Lexer
leftParen = spaced $ string "("
rightParen = spaced $ string ")"
leftBracket = spaced $ string "["
rightBracket = spaced $ string "]"
leftAngle = spaced $ string "<"
rightAngle = spaced $ string ">"
leftBrace = spaced $ string "{"
rightBrace = spaced $ string "}"

pcons = try $ do
  leftAngle
  t0 <- pterm
  t1 <- pterm
  rightAngle
  return $ TCons t0 t1

plist = try $ do
  leftParen
  ts <- many pterm
  rightParen
  return $ foldr1 cons ts
pright = try $ do
  leftBracket
  f <- pterm
  ts <- pterm
  rightBracket
  return $ lamed f ts
pleft = try $ do
  leftBrace
  f <- pterm
  ts <- pterm
  rightBrace
  -- rightBrace
  return $ rule f ts
spaced :: ParserG a -> ParserG a
-- spaced = lexeme space1
spaced parser = try $ do
  res <- parser
  try $ space
  -- space ;
  return res
pterm :: Parser
pterm =
  spaced $
    choice
      [ plist <?> "list"
      , pcons <?> "cons"
      , pleft <?> "left"
      , pright <?> "right"
      , try $ TAtom <$> lexT
      ]

psys :: ParserG [TestTerm]
psys = do
  res <- endBy1 pterm (spaced $ string ";")
  eof
  return res

-- | Note that lists are `()`, rules `{}`, and lameds `<>[]`
readTerm :: T.Text -> Maybe (TestTerm)
readTerm = parseMaybe pterm

readManyTerms = parseMaybe psys
readTermTest :: (Show TestTerm) => String -> IO ()
readTermTest input = parseTest pterm (T.pack input)
readTermsTest :: (Show TestTerm) => String -> IO ()
readTermsTest input = parseTest psys (T.pack input)
