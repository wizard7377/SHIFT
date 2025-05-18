{-# LANGUAGE QualifiedDo #-}

module Lift.Base.Mift.Parser where

import Control.Monad.Error.Class (MonadError (..))
import Control.Monad.RWS (MonadState (..), MonadTrans (..))
import Data.List (singleton)
import Extra
import Lift.Base.Core
import Lift.Base.Core.Forms.Module (currentModule, universe)
import Lift.Base.Core.Monad ()
import Lift.Base.Core.Symbol.Types
import Lift.Base.Mift.Lexer
import Lift.Base.Mift.Types
import Rift qualified
import Text.Megaparsec
import Text.Megaparsec qualified as T

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
