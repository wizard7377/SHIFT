{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Mift.Parse.Lexer
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Mift.Parse.Lexer where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Functor (($>))
import Data.Text qualified as T
import Lift.Common.Parsing (ParseMT)
import Text.Megaparsec qualified as P
import Text.Megaparsec qualified as T
import Text.Megaparsec.Char qualified as L hiding (space)
import Text.Megaparsec.Char.Lexer qualified as L

sc :: (Monad m, Ord e) => P.ParsecT e T.Text m ()
sc = L.space L.space1 empty empty

dollarSign :: [Char]
dollarSign = ['$']
spaces :: [Char]
spaces = [' ', '\t', '\n', '\r']
specials :: [Char]
specials = spaces <> dollarSign
lexeme :: (Ord e0, Monad m0) => P.ParsecT e0 T.Text m0 a0 -> P.ParsecT e0 T.Text m0 a0
lexeme = L.lexeme sc
symbol :: (Ord e0, Monad m0) => T.Text -> (P.ParsecT e0 T.Text m0 T.Text)
symbol = L.symbol sc
keyword :: (Ord e0, Monad m0) => T.Text -> P.ParsecT e0 T.Text m0 ()
keyword s = T.try $ symbol ("$" <> s) $> ()
glyph :: (Ord e, Monad m) => P.ParsecT e T.Text m T.Text
glyph = lexeme ((T.pack <$> some (T.noneOf specials)) <* (T.lookAhead $ T.oneOf spaces <|> (T.oneOf dollarSign)))
