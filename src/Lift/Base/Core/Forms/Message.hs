module Lift.Base.Core.Forms.Message where

import Control.Exception (SomeException)
import Extra

-- | The severity of a message
data Severity = Error | Warning | Info
  deriving (Eq, Show, Ord, Generic)

data Message = Message
  { _severity :: Severity
  , _message :: SomeException
  }
  deriving (Show, Generic)
