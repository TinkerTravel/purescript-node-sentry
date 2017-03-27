module Control.Logger.Node.Sentry
  ( nodeSentry
  ) where

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Exception (Error)
import Control.Logger (Logger(..))
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Node.Sentry (Attributes, Client, Level, NODESENTRY, captureError, captureMessage)
import Prelude

nodeSentry
  :: ∀ eff
   . Client
  -> Logger (Aff (nodeSentry :: NODESENTRY | eff))
            (Tuple (Either Error String)
                   (Attributes Level))
nodeSentry c = Logger case _ of
  Tuple (Left  e) a -> void $ captureError   c e a
  Tuple (Right m) a -> void $ captureMessage c m a
