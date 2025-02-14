{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Rift.Parser where
import Text.Megaparsec(ParsecT, choice, noneOf, MonadParsec (notFollowedBy, try), many, some, single, oneOf,parseTest,anySingle)
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer(lexeme)

import Data.Void
import Data.Functor.Identity
import Text.Megaparsec.Char
import Rift.Base
import Data.List (singleton)
import Control.Monad (void)
import Text.Megaparsec.Debug
import Data.Functor (($>))
type ParserG a = ParsecT Void String Identity a
type Parser = ParsecT Void String Identity (Term String)
type Lexer = ParsecT Void String Identity String
numbers :: [Char]
numbers = ['0'..'9']
letters :: [Char]
letters = ['a'..'z'] ++ ['A'..'Z']
specialChars :: [Char]
specialChars = "():?*!{}<>[]"
otherChar :: Char -> Bool
otherChar v = not (elem v numbers || elem v letters || elem v specialChars)


glyphP :: Parser
glyphP = do {
    res <- spaced (
        some (oneOf (letters ++ numbers))
        <|> some (satisfy otherChar)
    ) ;
    return $ Atom res
}
leftParen :: Lexer
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

plamed = try $ do {
    llamed ;
    leftAngle ;
    bound <- pterm ;
    rightAngle ;
    leftBracket ;
    term <- pterm ;
    rightBracket ;
    return $ Lamed bound term
} 
prule = try $ do {
    leftBrace ;
    t0 <- pterm ;
    colon ; 
    t1 <- pterm ;
    rightBrace ;
    return $ Rule t0 t1 
}
plist :: Parser
plist = try $ do {
    leftParen ;
    l <- some pterm ;
    rightParen ;
    return $ List l 
}
pyud = try $ Yud <$ lyud
spaced :: ParserG a -> ParserG a
--spaced = lexeme space1 
spaced parser = do {
    space ;
    res <- parser ;
    --space ;
    return res
}
pterm :: Parser
pterm = 
    choice [
        prule,
        plist,
        plamed,
        pyud,
        glyphP
    ]


readTerm :: String -> Maybe (Term String)
readTerm = parseMaybe pterm

