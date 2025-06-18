module Lift.Common.Parsing where

import Control.Monad.Except (MonadError (..))
import Control.Monad.RWS (MonadWriter (..))
import Control.Monad.RWS qualified as M
import Data.Text qualified as T
import Extra
import Lift.Common.Module
import Lift.Common.Names
import Rift qualified
import Text.Megaparsec qualified as P
import Text.Megaparsec.Char qualified as L

{- | The type of LParse monad
First arguemetn is error type, second is term type, third is monad
In addition takes a fourth is action
-}
type ParseMT :: Type -> Type -> (Type -> Type) -> (Type -> Type)
type ParseMT (e :: Type) (t :: Type) (m :: Type -> Type) = P.ParsecT e T.Text (M.RWST Rift.ParseEnv [e] (ParseState t) m)

type AnyParse a = forall e m. (Ord e, Monad m) => P.ParsecT e T.Text m a

data Lexical a = Lexical FPos a

lexInfo :: (Ord e, Monad m) => P.ParsecT e T.Text m a -> P.ParsecT e T.Text m (Lexical a)
lexInfo p = do
  start <- P.getSourcePos
  res <- p
  end <- P.getSourcePos
  pure $ Lexical (fromPos start end) res
