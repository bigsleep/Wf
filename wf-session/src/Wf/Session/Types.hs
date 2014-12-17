{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, DeriveGeneric, TypeFamilies, TemplateHaskell, StandaloneDeriving, ExistentialQuantification #-}
module Wf.Session.Types
( SessionState(..)
, SessionData(..)
, SessionSettings(..)
, SessionKvs(..)
, SessionError(..)
, SessionHandler(..)
, defaultSessionState
, defaultSessionData
, defaultSessionSettings
) where

import qualified Control.Exception (Exception(..))
import qualified Wf.Control.Eff.Kvs as Kvs (KeyType)
import Control.Applicative ((<$>), (<*>))

import Data.Binary (Binary(..))
import Data.Typeable (Typeable)
import qualified Data.HashMap.Strict as HM (HashMap, empty, fromList, toList)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import GHC.Generics (Generic)

import qualified Wf.Application.Time as T (Time, mjd)

data SessionState = SessionState
    { sessionId :: B.ByteString
    , sessionData :: SessionData
    , isNew :: Bool
    } deriving (Show, Typeable, Generic)

data SessionData = SessionData
    { sessionValue :: HM.HashMap B.ByteString L.ByteString
    , sessionStartDate :: T.Time
    , sessionExpireDate :: T.Time
    } deriving (Show, Typeable, Generic)

data SessionSettings = SessionSettings
    { sessionName :: B.ByteString
    , sessionIsSecure :: Bool
    , sessionTtl :: Integer
    , sessionIdLength :: Integer
    } deriving (Show, Typeable, Generic)

instance Binary SessionState

instance Binary SessionSettings

instance Binary SessionData where
    put (SessionData m s e) = put (HM.toList m) >> put s >> put e
    get = SessionData <$> (HM.fromList <$> get) <*> get <*> get

defaultSessionState :: SessionState
defaultSessionState = SessionState "" defaultSessionData False

defaultSessionData :: SessionData
defaultSessionData = SessionData HM.empty T.mjd T.mjd

defaultSessionSettings :: SessionSettings
defaultSessionSettings = SessionSettings "SID" False 3600 40

data SessionKvs = SessionKvs deriving (Typeable)

type instance Kvs.KeyType SessionKvs = B.ByteString

data SessionError =
    SessionError String
    deriving (Show, Eq, Typeable)

instance Control.Exception.Exception SessionError

data SessionHandler m = SessionHandler
    { sessionHandlerNew :: SessionSettings -> T.Time -> m SessionState
    , sessionHandlerLoad :: T.Time -> Maybe B.ByteString -> m SessionState
    , sessionHandlerSave :: T.Time -> SessionState -> m ()
    , sessionHandlerDestroy :: B.ByteString -> m ()
    } deriving (Typeable)