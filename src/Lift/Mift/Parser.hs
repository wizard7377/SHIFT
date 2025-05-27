{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QualifiedDo #-}

module Lift.Mift.Parser where

import Control.Lens (use)
import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState (..), MonadTrans (..))
import Data.Default
import Data.Foldable (traverse_)
import Data.List (singleton)
import Extra hiding (choice)
import Lift.Core
import Lift.Core.Forms.Module (Universe, currentModule, universe)
import Lift.Core.Monad ()
import Lift.Core.Symbol.Types
import Lift.Mift.Lexer
import Lift.Mift.Types
import Rift qualified
import Text.Megaparsec
import Text.Megaparsec qualified as T

type Parser a m r = (Monad m, MonadFail m) => PFMT (Universe (MiftTerm)) m r
oneOrMore :: PFMT t m a -> PFMT t m [a]
oneOrMore p =
  T.choice
    [ do
        try leftBrace
        res <- some p
        rightBrace
        pure res
    ]

-- | Get a pre-existing symbol from the symbol table
psymbol :: (Monad m, MonadFail m) => PFMT t m Symbol
psymbol = do
  (res, pos) <- withPos lexeme
  state <- get
  sym <- getSymbol (QName (state ^. currentModule) res)
  sym

-- | Parse simple definitions
pdefinesym :: (Monad m, MonadFail m) => PFMT t m ()
pdefinesym = do
  keyword "create"
  res <- plistOf (withPos lexeme)
  currentModule <- use currentModule
  let syms = (\(sym, loc) -> ((QName currentModule sym), mkSymbol sym loc def)) <$> res
  traverse_ (lift . (uncurry $ createSymbol)) syms
  pure ()

pterm :: PFMT t m (MiftTerm)
pterm =
  choice
    [ MiftAtom <$> psymbol
    , pcons
    , pparexpr
    ]

pcons :: PFMT t m (MiftTerm)
pcons = (keyword "." <|> keyword "cons") >> MiftList <$> (count 2 pterm)
pparexpr = (keyword "open" <|> keyword "(") >> MiftList <$> (manyTill pterm (keyword ")" <|> keyword "close"))
plistOf :: PFMT t m b -> PFMT t m [b]
plistOf e = do
  keyword "begin" <|> keyword "{"
  manyTill e (keyword "end" <|> keyword "}")
plistOfLen :: Int -> PFMT t m b -> PFMT t m [b]
plistOfLen n e = do
  keyword "begin" <|> keyword "{"
  res <- count n e
  keyword "end" <|> keyword "}"
  pure res
pdecl =
  choice
    [ pdefinesym <* pender
    ]
pmod = _
pfile = _
pinclude = _
paxiom = _
pproof = _
ptheorem = _
pender = keyword "end" <|> keyword ";"
