{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Rift.Core.Dev.Parser where

import Control.Monad (void)
import Control.Monad.State (MonadState (..), State, evalState)
import Data.Foldable (Foldable (..))
import Data.Functor (($>))
import Data.Functor.Identity
import Data.List (singleton)
import Data.Text qualified as T
import Data.Void
import Extra.List
import Rift.Core.Base
import Rift.Core.Dev.Lexer qualified as L
import Text.Megaparsec
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParsecT, anySingle, choice, many, noneOf, oneOf, parseTest, single, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer (lexeme)
import Text.Megaparsec.Debug

type Parser r = ParsecT Void [L.LexToken] (Control.Monad.State.State Int) r

parseCons :: Parser TestTerm
parseLamed :: Parser TestTerm
parseToken :: Parser TestTerm
-- parseParenList :: Parser TestTerm
pterm :: Parser TestTerm
parseCons = do
  single L.LParen
  t0 <- pterm
  t1 <- someTill pterm (single L.RParen)
  return $ foldr1 cons (t0 : t1)

parseLamed = do
  single L.LBracket
  vars <- pterm
  single L.RBracket
  single L.LBrace
  t0 <- pterm
  t1 <- pterm
  single L.RBrace
  return $ lamed vars t0 t1

parseToken = try $ do
  tok <- anySingle
  tv <- case tok of
    L.TokenValue s -> pure $ AAtom (Left $ T.pack s)
    L.TokenLamed -> pure $ ALamed
    L.TokenWild -> do
      current :: Int <- get
      put (current + 1)
      pure $ AAtom (Right current)
  pure $ TAtom tv

psys :: Parser [TestTerm]
psys = do
  res <- endBy1 pterm $ single L.TokenSep
  eof
  return res

pterm = parseLamed <|> parseCons <|> parseToken

parseOf :: Parser r -> T.Text -> Maybe r
parseOf parser input = case evalState (runParserT parser "TEST" (L.lexTest $ T.unpack input)) 0 of
  Left _ -> Nothing
  Right res -> Just res

-- | Note that lists are `()`, rules `{}`, and lameds `<>[]`
readTerm :: String -> Maybe TestTerm
readTerm = (parseOf pterm) . T.pack

readManyTerms = (parseOf psys) . T.pack
