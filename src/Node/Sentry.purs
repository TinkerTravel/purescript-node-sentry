module Node.Sentry
  ( NODESENTRY

  , Client
  , newClient

  , captureError
  , captureMessage
  ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Prelude

--------------------------------------------------------------------------------

foreign import data NODESENTRY :: Effect

--------------------------------------------------------------------------------

foreign import data Client :: Type

newClient :: ∀ eff. String -> Aff (nodeSentry :: NODESENTRY | eff) Client
newClient = liftEff <<< newClientFFI

foreign import newClientFFI
  :: ∀ eff. String -> Eff (nodeSentry :: NODESENTRY | eff) Client

--------------------------------------------------------------------------------

captureError
  :: ∀ eff
   . Client
  -> Error
  -> Aff (nodeSentry :: NODESENTRY | eff) String
captureError = (makeAff <<< _) <<< captureErrorFFI

captureMessage
  :: ∀ eff
   . Client
  -> String
  -> Aff (nodeSentry :: NODESENTRY | eff) String
captureMessage = (makeAff <<< _) <<< captureMessageFFI

foreign import captureErrorFFI
  :: ∀ eff
   . Client
  -> Error
  -> (Error -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> (String -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> Eff (nodeSentry :: NODESENTRY | eff) Unit

foreign import captureMessageFFI
  :: ∀ eff
   . Client
  -> String
  -> (Error -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> (String -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> Eff (nodeSentry :: NODESENTRY | eff) Unit
