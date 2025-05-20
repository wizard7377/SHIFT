{-# LANGUAGE QualifiedDo #-}

module Lift.Mift.Parser where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState (..), MonadTrans (..))
import Data.List (singleton)
import Extra
import Lift.Core
import Lift.Core.Forms.Module (Universe, currentModule, universe)
import Lift.Core.Monad ()
import Lift.Core.Symbol.Types
import Lift.Mift.Lexer
import Lift.Mift.Types
import Rift qualified
import Text.Megaparsec
import Text.Megaparsec qualified as T

type Parser a r = forall m. (Monad m, MonadFail m) => PFMT (Universe (MiftTerm a)) m r
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
psymbol :: (Monad m, MonadFail m) => PFMT t m (MiftTerm t)
psymbol = do
  (res, pos) <- withPos lexeme
  state <- get
  -- sym <- getSymbol (QName (state ^. currentModule) res)
  _

pterm :: Parser a (MiftTerm a)
pterm = _
plistOf :: Parser a b -> Parser a [b]
plistOf e = _
pdecl = _
pmod = _
pfile = _
pinclude = _
paxiom = _
pproof = _
ptheorem = _
