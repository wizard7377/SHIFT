{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Tift.Parser
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Tift.Parser where

import Control.Monad (join, void)
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

import Control.Lens ((+=))
import Control.Lens.Operators ((%=))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Morph (MFunctor (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.List.Extra
import Data.Text qualified as T
import Data.Void (Void)
import Extra
import Lift.Tift.Expr
import Lift.Tift.Lexer
import Lift.Tift.Types
import Rift qualified
import System.Directory.Extra (getCurrentDirectory)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), Parsec, ParsecT, anySingle, choice, eof, many, noneOf, oneOf, parseTest, single, some, try, (<|>))
import Text.Megaparsec hiding (EndOfInput, State)
import Text.Megaparsec.Char
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug
import Prelude hiding (lex)

lamedP :: Parser (TiftTerm)
lamedP = special ["lam"] $> TiftLamed

maybeMany :: Parser TiftTerm
maybeMany = do
  termA <- termP
  termB <- optional $ try $ some termP
  case termB of
    Just terms -> pure $ foldr1 (\acc x -> TiftCons acc x) (termA : terms)
    Nothing -> pure termA
wildP :: Parser (TiftTerm)
wildP = do
  try $ symbol "_"
  n <- get
  cindex += 1
  pure $ TiftWild (n ^. cindex)

pvoid :: Parser TiftTerm
pvoid = do
  _ <- special ["bad"]
  pure TiftError
kafP :: Parser (TiftTerm)
kafP =
  do
    _ <- symbol "("
    firstOf <- termP
    restOf <- many termP
    symbol ")"
    pure $
      foldr1
        (\acc x -> TiftCons acc x)
        (firstOf : restOf)

consP :: Parser (TiftTerm)
consP = (try $ symbol ".") *> termP

freeP :: Parser (TiftTerm)
freeP = do
  frees <- keyword ["var"] termP
  term <- termP
  pure $ TiftFree term frees

repP = try $ do
  [termFrom, termTo] <- keyword ["rep"] termP
  within <- termP
  pure $ TiftRep termFrom termTo within
termP :: Parser (TiftTerm)
termP = choice [freeP, kafP, lamedP, wildP, (TiftAtom <$> atomicP), feP, peP, repP, pvoid, consP]

peP :: Parser (TiftTerm)
peP =
  label "Fixpoint" $
    try
      ( do
          v : _ <- keyword ["fix"] atomicP
          state <- get
          cindex += 1
          namedScopes . mAt v .= (pure $ v >-> (state ^. cindex))
          term <- termP
          pure $ TiftPe (state ^. cindex) term
      )
      <|> ( do
              special ["fix"]
              state <- get
              cindex += 1
              unnamedScope .= (state ^. cindex)
              term <- termP
              pure $ TiftPe (state ^. cindex) term
          )

feP :: Parser (TiftTerm)
feP =
  label "Recursion" $
    try
      ( do
          v : _ <- keyword ["rec"] atomicP
          state <- get
          let [(TImage _ _ i)] = state ^. namedScopes . mAt v
          pure $ TiftFe i
      )
      <|> ( do
              special ["rec"]
              state <- get
              pure $ TiftFe (state ^. unnamedScope)
          )

pdef :: Parser ()
pdef = label "Definition" $ do
  _ <- hidden $ keyword ["def"] $ fail ""
  (name :: T.Text) <- T.pack <$> (lexeme $ some normal)
  definition <- termP
  theory . Rift.defines %= (<> (TMap $ singleton $ TImage name (TiftAtom name) definition))
  pure ()
pthm :: Parser ()
pthm = label "Theorem" $ do
  name : _ <- hidden $ keyword ["thm"] $ atomicP
  lhs <- termP
  rhs <- termP
  theory . Rift.proofs %= (<> (TMap $ pure $ TImage name lhs rhs))

pdeclaration :: Parser ()
pdeclaration = (pdef <|> pthm) <* symbol ";"
programP :: Parser (Rift.SimpleTheory () T.Text TiftTerm)
programP = do
  optional sc
  manyTill (try pdeclaration) eof

  state <- get
  pure $ state ^. theory
parseTerm :: T.Text -> IO (TiftTerm)
parseTerm input = do
  let (result, _) = runState (runParserT (termP <* eof) "" input) def
  case result of
    Left err -> error $ "Parse error: " ++ errorBundlePretty err
    Right term -> pure term

parseFile ::
  Rift.ParseEnv ->
  T.Text ->
  IO (Rift.SimpleTheory () T.Text TiftTerm)
parseFile e s = do
  let fileName = e ^. Rift.cwd <> "/" <> (T.unpack s)
  fileContent <- readFile fileName
  let (result, _) = runState (runParserT programP (fileName) $ T.pack fileContent) def
  case result of
    Left err -> error $ "Parse error: " ++ errorBundlePretty err
    Right term -> pure term
parseText ::
  Rift.ParseEnv ->
  T.Text ->
  IO (TiftTerm)
parseText _ s = do
  let (result, _) = runState (runParserT termP "" $ (s)) def
  case result of
    Left err -> error $ "Parse error: " ++ errorBundlePretty err
    Right term -> pure term

parseTermTest :: String -> IO TiftTerm
parseTermTest input = do
  cwd' <- liftIO getCurrentDirectory
  parseText (Rift.ParseEnv cwd') (T.pack input)
parseFileTest :: String -> IO (Rift.SimpleTheory () T.Text TiftTerm)
parseFileTest input = do
  cwd' <- getCurrentDirectory
  program <- parseFile (Rift.ParseEnv cwd') $ T.pack input
  pure $ program
