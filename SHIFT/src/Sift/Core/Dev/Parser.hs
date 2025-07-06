{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Sift.Core.Dev.Parser
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Sift.Core.Dev.Parser (Parser, sc, symbol, lexeme, termP, parseTerm) where

import Control.Monad (void)
import Control.Monad.State (MonadState (..), State, StateT (runStateT), evalState, runState)
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
import Data.List.Extra
import Data.Text qualified as T
import Data.Void (Void)
import Extra
import Rift qualified
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, ParsecT, anySingle, choice, eof, many, noneOf, oneOf, parseTest, single, some, try, (<|>))
import Text.Megaparsec hiding (EndOfInput, State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Prelude hiding (lex)

type Parser = ParsecT Void T.Text (State Int)
sc :: Parser ()
sc = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")
symbol :: T.Text -> Parser T.Text
symbol = L.symbol sc
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

atomicP :: Parser (Rift.TestTerm)
atomicP = lexeme $ do
  tok <- some (noneOf (" \n\t()[]{}?" :: String))
  pure $ Rift.PrimAtom $ TestToken $ Left $ T.pack tok

lamedP :: Parser (Rift.TestTerm)
lamedP = try (symbol "?") $> Rift.PrimLamed

wildP :: Parser (Rift.TestTerm)
wildP = do
  try $ symbol "_"
  n <- get
  put (n + 1)
  pure $ Rift.PrimAtom $ TestToken $ Right n

kafP :: Parser (Rift.TestTerm)
kafP =
  do
    _ <- symbol "("
    firstOf <- termP
    restOf <- many termP
    symbol ")"
    pure $
      foldr1
        (\acc x -> Rift.PrimCons acc x)
        (firstOf : restOf)

freeP :: Parser (Rift.TestTerm)
freeP = do
  _ <- symbol "["
  frees <- some termP
  _ <- symbol "]"
  term <- termP
  pure $ PrimFree term frees

repP = do
  _ <- symbol "{"
  termFrom <- termP
  termTo <- termP
  _ <- symbol "}"
  pure $ PrimRep termFrom termTo
termP :: Parser (Rift.TestTerm)
termP = choice [freeP, kafP, lamedP, wildP, atomicP]

parseTerm :: T.Text -> IO (Rift.TestTerm)
parseTerm input = do
  let (result, _) = runState (runParserT (termP <* eof) "" input) 0
  case result of
    Left err -> error $ "Parse error: " ++ errorBundlePretty err
    Right term -> pure term
