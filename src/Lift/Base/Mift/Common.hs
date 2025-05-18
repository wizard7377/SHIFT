{-# LANGUAGE OverloadedStrings #-}

module Lift.Base.Mift.Common where

import Data.Text qualified as T
import Text.Megaparsec
import Text.Megaparsec.Char (char)

dollarSymbol :: [Token T.Text]
dollarSymbol = ['$']
whiteSpace :: [Token T.Text]
whiteSpace = [' ', '\t', '\n', '\r']
specialSymbol :: [Token T.Text]
specialSymbol = dollarSymbol ++ whiteSpace
spaceLex0 :: (Ord e) => ParsecT e T.Text m ()
spaceLex0 = skipMany $ oneOf whiteSpace
spaceLex1 :: (Ord e) => ParsecT e T.Text m ()
spaceLex1 = skipSome $ oneOf whiteSpace
spacing :: (Ord e) => ParsecT e T.Text m a -> ParsecT e T.Text m a
spacing p = do
  spaceLex0
  res <- p
  spaceLex1
  pure res
