module Hift.Toplevel where

import Control.Monad.RWS qualified as M
import Control.Monad.State qualified as M
import Control.Monad.Trans (MonadTrans (..))
import Extra
import Hift.Opts.Types
import Rift qualified
import System.Console.Haskeline (InputT, defaultSettings, runInputT)

type HiftM t = InputT (M.RWST ProgOpts () t IO)

runHiftM :: HiftM t a -> ProgOpts -> t -> IO a
runHiftM action opts state = fst <$> (M.evalRWST (runInputT defaultSettings action) opts state)

hiftHandle :: HiftM t t
hiftHandle = do
  opts <- lift M.ask
  state <- lift M.get
  case opts ^. action of
    Repl _ -> do
      topLevel
    _ -> _
topLevel :: HiftM t t
topLevel = _
