{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK show-extensions, prune #-}

{- |
Module      : Lift.Mift.Parse.Parser
License     : BSD-2-Clause
Maintainer  : Asher Frost
-}
module Lift.Mift.Parse.Parser where

import Control.Applicative (Alternative (..))
import Control.Lens qualified as Lens
import Control.Lens.Operators ((^..))
import Control.Monad qualified as M
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.RWS qualified as M
import Control.Monad.Trans
import Control.Monad.Trans qualified as M
import Data.Functor (($>))
import Data.Text qualified as T
import Extra
import Lift.Common.Module
import Lift.Common.Names (FPath (..), Name, nameText, toName)
import Lift.Common.Parsing (lexInfo)
import Lift.Common.Tokens (Arity (..), MathToken (..), TokenValue (..))
import Lift.Mift.Base
import Lift.Mift.Expr
import Lift.Mift.Parse.Lexer
import Lift.Mift.Parse.State
import Rift qualified
import Text.Megaparsec ()
import Text.Megaparsec qualified as T hiding ((<?>))
import Text.Megaparsec.Debug (dbg)

miftToplevel :: MiftM (Program MiftExpr)
miftFile :: FilePath -> MiftM ()
miftDecl :: MiftM ()
miftExpr :: MiftM MiftExpr
miftModDeclare :: MiftM ()
miftSymDeclare :: MiftM ()
miftProofDeclare :: MiftM ()
miftAssertDeclare :: MiftM ()
miftLamed :: MiftM ()
miftLParen :: MiftM ()
miftRParen :: MiftM ()
miftLBrace :: MiftM ()
miftRBrace :: MiftM ()
miftConsDot :: MiftM ()
miftEqual :: MiftM ()
miftSkip :: MiftM ()
miftAssert :: MiftM ()
miftSep :: MiftM ()
miftWild :: MiftM ()
miftVoid :: MiftM ()

-- | Create a name, assuming it is already defined
miftAtomUse :: MiftM Name

miftToplevel = do
  miftFile'
  T.eof
  state <- lift $ M.get
  pure (state ^. programState)

-- | Go to a file, parse it, then restore the state
miftFile path = do
  state <- M.get
  progres <- T.getInput
  currentFile .= FPath path
  fileContents <- liftIO $ readFile path
  T.setInput $ T.pack fileContents
  miftFile'
  T.setInput progres
  pure ()

miftFile' = do
  T.optional sc *> miftModDeclare
miftDecl =
  T.choice
    [ miftSymDeclare
    , miftProofDeclare
    , miftAssertDeclare
    ]
    <* (T.many miftSkip)
miftModDeclare = dbg "MODULE" $ do
  state <- M.get
  keyword "module"
  modname <- glyph
  if (not . null $ state ^. programState . modules . mAt (state ^. programState . currentModule))
    then do
      liftIO $
        print $
          show $
            state
              ^. programState
              . modules
              . mAt
                (state ^. programState . currentModule)
      liftIO (error "") -- FIXME:
    else do
      programState . modules . (mAt $ toName modname) .= (pure $ (toName modname) >-> (buildModule $ toName modname))
      programState . currentModule .= toName modname
      T.many miftDecl
      pure ()
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
miftProofDeclare = do
  name <- T.try (miftAssert *> glyph <* miftSep)
  sideA <- miftExpr
  miftEqual
  sideB <- miftExpr
  mkProof (toName name) (TImage (toName name) sideA sideB)
miftAssertDeclare = do
  miftAssert
  defines <- glyph
  miftEqual
  value <- miftExpr
  mkAssert (toName defines) (image (toName defines) value)
miftAtomUse = do
  sym <- glyph
  _ <- getToken (toName sym)
  pure $ toName sym
miftExpr =
  T.choice
    [ T.label "Lamed expression" $ miftLamed $> MiftLamed
    , T.label "Parenthisized expression" (T.between miftLParen miftRParen (MiftList <$> T.some miftExpr))
    , T.label "Cons expression" (miftConsDot *> (MiftCons <$> miftExpr <*> miftExpr))
    , T.label "Atomic expression" (MiftAtom <$> miftAtomUse)
    , T.label "Wildcard expression" (miftWild $> MiftWild)
    , T.label "Void expression" (miftVoid $> MiftVoid)
    , miftTuple
    ]

miftTuple =
  T.label "Tuple expression" (T.between miftLBrace miftRBrace (MiftTuple <$> T.some miftExpr))
miftLamed = T.label "lamed (ל)" $ keyword "?"
miftLParen = T.label "left paren" $ keyword "("
miftRParen = T.label "right paren" $ keyword ")"
miftLBrace = T.label "left brace" $ keyword "{"
miftRBrace = T.label "right brace" $ keyword "}"
miftConsDot = T.label "cons dot" $ keyword "."
miftEqual = T.label "Equals sign" $ keyword "="
miftSkip = T.label "End of statement" $ keyword ";"
miftAssert = T.label "Assertion" $ keyword ":"
miftWild = T.label "Wildcard" $ keyword "_"
miftVoid = T.label "Wildcard" $ keyword "!"
miftSep = T.label "Seperator" $ keyword "-"
