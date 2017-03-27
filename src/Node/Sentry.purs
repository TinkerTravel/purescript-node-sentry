module Node.Sentry
  ( NODESENTRY

  , Client
  , newClient

  , Attributes
  , attributesUser
  , attributesTags
  , attributesExtra
  , attributesFingerprint
  , attributesLevel
  , defaultAttributes

  , Level(..)

  , captureError
  , captureMessage
  ) where

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (Error)
import Data.Argonaut.Core (Json)
import Data.Lens (Lens, Lens', lens)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
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

newtype Attributes l = Attributes
  { user        :: StrMap Json
  , tags        :: StrMap String
  , extra       :: StrMap Json
  , fingerprint :: Array String
  , level       :: l
  }

derive instance functorAttributes :: Functor Attributes

attributesUser :: ∀ l. Lens' (Attributes l) (StrMap Json)
attributesUser = lens get set
  where get (Attributes a) = a.user
        set (Attributes a) user = Attributes a {user = user}

attributesTags :: ∀ l. Lens' (Attributes l) (StrMap String)
attributesTags = lens get set
  where get (Attributes a) = a.tags
        set (Attributes a) tags = Attributes a {tags = tags}

attributesExtra :: ∀ l. Lens' (Attributes l) (StrMap Json)
attributesExtra = lens get set
  where get (Attributes a) = a.extra
        set (Attributes a) extra = Attributes a {extra = extra}

attributesFingerprint :: ∀ l. Lens' (Attributes l) (Array String)
attributesFingerprint = lens get set
  where get (Attributes a) = a.fingerprint
        set (Attributes a) fingerprint =
          Attributes a {fingerprint = fingerprint}

attributesLevel :: ∀ l l'. Lens (Attributes l) (Attributes l') l l'
attributesLevel = lens get set
  where get (Attributes a) = a.level
        set (Attributes a) level = Attributes a {level = level}

defaultAttributes :: Attributes Level
defaultAttributes = Attributes
  { user:        StrMap.empty
  , tags:        StrMap.empty
  , extra:       StrMap.empty
  , fingerprint: ["{{ default }}"]
  , level:       Error
  }

--------------------------------------------------------------------------------

data Level = Debug | Info | Warning | Error | Fatal

levelString :: Level -> String
levelString Debug   = "debug"
levelString Info    = "info"
levelString Warning = "warning"
levelString Error   = "error"
levelString Fatal   = "fatal"

--------------------------------------------------------------------------------

captureError
  :: ∀ eff
   . Client
  -> Error
  -> Attributes Level
  -> Aff (nodeSentry :: NODESENTRY | eff) String
captureError c e a = makeAff $ captureErrorFFI c e (map levelString a)

captureMessage
  :: ∀ eff
   . Client
  -> String
  -> Attributes Level
  -> Aff (nodeSentry :: NODESENTRY | eff) String
captureMessage c e a = makeAff $ captureMessageFFI c e (map levelString a)

foreign import captureErrorFFI
  :: ∀ eff
   . Client
  -> Error
  -> Attributes String
  -> (Error -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> (String -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> Eff (nodeSentry :: NODESENTRY | eff) Unit

foreign import captureMessageFFI
  :: ∀ eff
   . Client
  -> String
  -> Attributes String
  -> (Error -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> (String -> Eff (nodeSentry :: NODESENTRY | eff) Unit)
  -> Eff (nodeSentry :: NODESENTRY | eff) Unit
