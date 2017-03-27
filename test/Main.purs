module Test.Main
  ( main
  ) where

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Maybe (maybe)
import Node.Process (PROCESS, exit, lookupEnv)
import Node.Sentry (NODESENTRY, newClient, captureError, captureMessage)
import Prelude

type Effects eff =
  ( err        :: EXCEPTION
  , nodeSentry :: NODESENTRY
  , process    :: PROCESS
  | eff
  )

main :: ∀ eff. Eff (Effects eff) Unit
main = void $ launchAff do
  dsn <- liftEff $ maybe (exit 1) pure =<< lookupEnv "SENTRY_DSN"
  client <- newClient dsn
  captureError   client $ error "captureError"
  captureMessage client $ "captureMessage"
