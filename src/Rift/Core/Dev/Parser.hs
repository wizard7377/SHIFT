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
import Debug.Trace (trace)
import Extra.Debug (traceWithStr, (?>>))
import Extra.List
import Rift.Core.Base
import Rift.Core.Instances ()

-- import Rift.Core.Dev.Lexer qualified as L

import Control.Monad.Trans (MonadTrans (..))
import Text.Megaparsec
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParsecT, anySingle, choice, many, noneOf, oneOf, parseTest, single, some)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug

data LexToken
  = LBrace
  | RBrace
  | LBracket
  | RBracket
  | LAngle
  | RAngle
  | LParen
  | RParen
  | TokenLamed
  | TokenCons
  | TokenSep
  | TokenWild
  | TokenValue String
  deriving (Show, Eq, Ord)
type Parser r = ParsecT Void T.Text (Control.Monad.State.State Int) r

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment $ T.pack "//")
    (L.skipBlockCommentNested (T.pack "/*") (T.pack "*/"))
symbol s = L.symbol sc $ T.pack s
lexeme l = L.lexeme sc $ l
parseCons :: Parser TestTerm
parseLamed :: Parser TestTerm
parseToken :: Parser TestTerm
-- parseParenList :: Parser TestTerm
pterm :: Parser TestTerm
parseCons = label "List" $ do
  symbol "("
  t0 <- pterm
  t1 <- someTill pterm (symbol ")")
  return $ foldr1 cons (t0 : t1)

lamed' :: Term' atom -> (Term' atom, Term' atom) -> Term' atom
lamed' base (v, t) = lamed v base t
parseLamed = label "Lamed" $ do
  symbol "["
  vars <- many pterm
  symbol "]"
  symbol "{"
  t0 <- pterm
  ts <- count (length vars) pterm
  symbol "}"
  let combo = zip (reverse vars) ts
  return $ foldl lamed' t0 combo

parseToken = label "Token" $ do
  tok <- lexeme $ some (noneOf "(){}[]; ")
  v <- case tok of
    "_" -> do
      (i :: Int) <- get
      let v = TAtom (AAtom (TestToken $ Right i))
      put (i + 1)
      return v
    str -> return $ TAtom (AAtom (TestToken $ Left $ T.pack str))
  return v

psys' :: Parser [TestTerm]
psys' = label "System" $ do
  optional sc
  res <- endBy1 pterm $ symbol ";"
  return res

psys = do
  res <- psys'
  pure res
pterm = label "Term" $ choice [try parseLamed, try parseCons, parseToken]

pfile :: Parser [[TestTerm]]
pfile = label "File" $ do
  res <- endBy1 psys' $ symbol ";;;"
  pure res
parseOf :: Parser r -> T.Text -> Maybe r
parseOf parser input = case evalState (runParserT parser "TEST" (input)) 0 of
  Left e -> trace (errorBundlePretty e) Nothing
  Right res -> Just res

-- | Note that lists are `()`, rules `{}`, and lameds `<>[]`
readTerm :: String -> Maybe TestTerm
readTerm = (parseOf pterm) . T.pack

readTerm' = (parseOf (pterm <* eof)) . T.pack
readManyTerms = (parseOf psys) . T.pack
readManyTerms' = (parseOf (pfile)) . T.pack

parseTestOne = readTerm'
parseTestMany = readManyTerms'
