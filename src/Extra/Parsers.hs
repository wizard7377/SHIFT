{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Extra.Parsers where

import Control.Comonad
import Control.Exception
import Control.Monad.IO.Class
import Data.Functor.Identity (Identity)
import Data.Kind (Type)
import Data.String (IsString (..))
import Data.Void (Void)
import Text.Megaparsec

-- | Something that can be parsed
class (IsString (InputOf a), Monad (MonadOf a)) => Parsable (a :: Type) where
  type InputOf (a :: Type) :: Type
  type InputOf a = String
  type MonadOf (a :: Type) :: Type -> Type
  type MonadOf a = Identity
  type ErrorOf (a :: Type) :: Type
  type ErrorOf a = Void
  pread :: ParsecT (ErrorOf a) (InputOf a) (MonadOf a) a

preadWithIO :: (Exception (ErrorOf a), Comonad (MonadOf a), Parsable a, IsString (InputOf a), Stream (InputOf a), ShowErrorComponent (ErrorOf a), VisualStream (InputOf a), TraversableStream (InputOf a)) => String -> IO a
preadWithIO input = do
  let result = extract $ runParserT pread (fromString "") (fromString "")
  case result of
    Left err -> throw (userError $ errorBundlePretty err)
    Right val -> return val
