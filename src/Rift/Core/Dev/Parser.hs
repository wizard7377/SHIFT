{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}

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
import Data.List.Extra
import Extra
import Text.Megaparsec (MonadParsec (notFollowedBy, try), ParsecT, anySingle, choice, many, noneOf, oneOf, parseTest, single, some)
import Text.Megaparsec hiding (EndOfInput, State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Prelude hiding (lex)

data LexToken
  = LParen
  | RParen
  | LamedTok
  | WildTok
  | SymbolTok T.Text
  deriving (Eq, Show, Ord, Data, Typeable, Generic)

lex :: T.Text -> [LexToken]
lex input =
  ( \(x :: T.Text) -> case x of
      "?" -> LamedTok
      "_" -> WildTok
      "(" -> LParen
      ")" -> RParen
      _ -> SymbolTok x
  )
    <$> lw
 where
  lw = T.words input

getUpTo :: (Eq a) => a -> a -> Int -> [a] -> Maybe ([a], [a])
getUpTo open close depth (x : xs) =
  if x == open
    then case getUpTo open close (depth + 1) xs of
      Just (ys, zs) -> Just (x : ys, zs)
      Nothing -> Nothing
    else
      if x == close
        then
          if depth <= 1
            then Just ([], xs)
            else case getUpTo open close (depth - 1) xs of
              Just (ys, zs) -> Just (x : ys, zs)
              Nothing -> Nothing
        else case getUpTo open close depth xs of
          Just (ys, zs) -> Just (x : ys, zs)
          Nothing -> Nothing

data NoParse
  = ParseError
  | EndOfInput
  deriving (Show, Eq)
makeWild :: State Int TestTerm
makeWild = do
  n <- get
  put (n + 1)
  pure $ PrimAtom (TestToken (Right n))

appendTerm :: (Either NoParse TestTerm) -> Either NoParse TestTerm -> Either NoParse TestTerm
appendTerm (Left ParseError) _ = Left ParseError
appendTerm (Left EndOfInput) b = b
appendTerm (Right a) (Left ParseError) = Left ParseError
appendTerm (Right a) (Left EndOfInput) = Right a
appendTerm (Right a) (Right b) = Right (Kaf a b)
parseTerm' :: [LexToken] -> State Int (Either NoParse TestTerm)
parseTerm' tokens = case tokens of
  (LParen : rest) -> case getUpTo LParen RParen 1 rest of
    Just (inner, rest') -> do
      term <- parseTerm' inner
      restTerms <- parseTerm' rest'
      pure $ appendTerm term restTerms
    Nothing -> pure $ Left ParseError
  WildTok : rest -> do
    term <- makeWild
    rests <- (parseTerm' rest)
    pure $ appendTerm (Right term) rests
  SymbolTok sym : rest -> do
    let term = PrimAtom (TestToken (Left sym))
    rests <- parseTerm' rest
    pure $ appendTerm (Right term) rests
  RParen : _ -> pure $ Left ParseError
  [] -> pure $ Left EndOfInput

parseTerm :: String -> Maybe TestTerm
parseTerm input =
  case evalState (parseTerm' (lex (T.pack input))) 0 of
    Left ParseError -> Nothing
    Left EndOfInput -> Nothing
    Right term -> Just term
