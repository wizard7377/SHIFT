{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}
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
preadWithIO :: forall a. (Parsable a, Monad (MonadOf a)) => String -> (MonadOf a) a
preadWithIO input = do
  (result :: (Either _ a)) <- runParserT pread (fromString "") (T.pack input)
  case result of
    Left err -> throw (userError $ errorBundlePretty err)
    Right val -> return val
