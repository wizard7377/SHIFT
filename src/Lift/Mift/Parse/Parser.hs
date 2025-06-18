{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Lift.Mift.Parse.Parser where

import Control.Applicative (Alternative (..))
import Control.Lens qualified as Lens
import Control.Lens.Operators ((^..))
import Control.Monad qualified as M
import Control.Monad.RWS qualified as M
import Control.Monad.Trans qualified as M
import Data.Functor (($>))
import Extra
import Lift.Common.Module
import Lift.Common.Names (nameText, toName)
import Lift.Common.Parsing (lexInfo)
import Lift.Common.Tokens (Arity (..), MathToken (..), TokenValue (..))
import Lift.Mift.Base
import Lift.Mift.Expr
import Lift.Mift.Parse.Lexer
import Lift.Mift.Parse.State
import Text.Megaparsec qualified as T hiding ((<?>))

miftToplevel :: MiftM ()
miftFile :: MiftM ()
miftDecl :: MiftM ()
miftExpr :: MiftM MiftExpr
miftModDeclare :: MiftM _
miftSymDeclare :: MiftM ()
miftProofDeclare :: MiftM _
miftAssertDeclare :: MiftM _
miftLamed :: MiftM ()
miftLParen :: MiftM ()
miftRParen :: MiftM ()
miftLBrace :: MiftM ()
miftRBrace :: MiftM ()
miftConsDot :: MiftM ()
miftToplevel = _
miftFile = _
miftDecl = _
miftModDeclare = do
  state <- M.get
  keyword "module"
  modname <- glyph
  if (not . null $ state ^.. programState . modules . mAt (state ^. currentModule))
    then
      _
    else do
      programState . modules . mAt (state ^. currentModule) .= def
      T.many miftDecl
miftSymDeclare = do
  state <- M.get
  keyword "symbol"
  T.some
    ( ( T.try $ do
          tup <- miftTuple
          _
      )
        <|> ( do
                gly <- glyph
                mkToken (toName gly) ((image $ toName gly) $ MathValue $ MathToken (toName gly) (Arity 0 0 1))
            )
    )
  pure ()
miftProofDeclare = _
miftAssertDeclare = _
miftExpr =
  T.choice
    [ T.label "Lamed expression" $ miftLamed $> MiftLamed
    , T.label "Parenthisized expression" (T.between miftLParen miftRParen (MiftList <$> T.some miftExpr))
    , T.label "Cons expression" (miftConsDot *> (MiftCons <$> miftExpr <*> miftExpr))
    , T.label "Atomic expression" (MiftAtom <$> toName <$> glyph)
    , miftTuple
    ]

miftTuple :: MiftM MiftExpr
miftTuple =
  T.label "Tuple expression" (T.between miftLBrace miftRBrace (MiftTuple <$> T.some miftExpr))
miftLamed = T.label "lamed (ל)" $ keyword "?"
miftLParen = T.label "left paren" $ keyword "("
miftRParen = T.label "right paren" $ keyword ")"
miftLBrace = T.label "left brace" $ keyword "{"
miftRBrace = T.label "right brace" $ keyword "}"
miftConsDot = T.label "cons dot" $ keyword "."
