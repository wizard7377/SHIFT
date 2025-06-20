{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.Parsers where

import Control.Comonad
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.State qualified as M
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Text qualified as T
import Data.Void (Void)
import Text.Megaparsec

-- | Something that can be parsed
class Parsable (a :: Type) where
  type MonadOf a :: Type -> Type
  type MonadOf a = Identity
  pread :: ParsecT Void T.Text (MonadOf a) a

instance Comonad (M.State Int) where
  extract s = M.evalState s 0
  duplicate = pure
preadWithIO :: (MonadOf a ~ w, Monad w, Comonad w, MonadIO m, Parsable a) => String -> m a
preadWithIO input = do
  let result = extract $ runParserT pread (fromString "") (T.pack input)
  case result of
    Left err -> throw (userError $ errorBundlePretty err)
    Right val -> return val
