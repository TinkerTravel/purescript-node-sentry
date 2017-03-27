module Test.Main
  ( main
  ) where

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Data.Lens ((.~))
import Data.Maybe (maybe)
import Node.Process (PROCESS, exit, lookupEnv)
import Node.Sentry
import Prelude (Unit, (#), ($), (=<<), bind, pure, unit, void)

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
  let attributes f = defaultAttributes # attributesLevel       .~ Info
                                       # attributesFingerprint .~ f
  _ <- captureMessage client "captureMessage"       (attributes ["message"])
  _ <- captureError   client (error "captureError") (attributes ["error"])
  pure unit
