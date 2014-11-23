{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
module Wf.Control.Eff.Run.Session.Kvs
( runSessionKvs
) where

import qualified Data.ByteString as B (ByteString)

import Control.Eff (Eff, (:>), Member, SetMember)
import qualified Control.Eff.State.Strict as State (State)
import Control.Eff.Lift (Lift, lift)
import Wf.Control.Eff.Session (Session(..))
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs, get, setWithTtl, delete, exists)

import Control.Monad (when, void)
import Control.Applicative ((<$>), (<*>))

import Wf.Web.Session.Types (SessionHandler(..), SessionState(..), SessionData(..), SessionSettings(..), SessionKvs(..), defaultSessionState, defaultSessionData)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug, logInfo)
import qualified Wf.Application.Time as T (Time, addSeconds, diffTime)
import Wf.Control.Eff.Run.Session (genSessionId, runSession)

import Text.Printf.TH (s)


runSessionKvs
    ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , Member (State.State SessionState) r
    , SetMember Lift (Lift IO) r
    )
    => SessionSettings
    -> T.Time
    -> Maybe B.ByteString
    -> Eff (Session :> r) a
    -> Eff r a
runSessionKvs = runSession sessionHandlerKvs



sessionHandlerKvs
    ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , SetMember Lift (Lift IO) r
    )
    => SessionHandler (Eff r)
sessionHandlerKvs = SessionHandler
    { sessionHandlerNew = newSession
    , sessionHandlerLoad = loadSession
    , sessionHandlerSave = saveSession
    , sessionHandlerDestroy = destroySession
    }



newSession
    ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , SetMember Lift (Lift IO) r
    )
    => SessionSettings
    -> T.Time
    -> Eff r SessionState
newSession sessionSettings current = do
    sid <- lift $ genSessionId sname current (fromInteger len)
    duplicate <- Kvs.exists SessionKvs sid
    if duplicate
        then newSession sessionSettings current
        else do
             let start = current
             let end = T.addSeconds start ttl
             let sd = defaultSessionData { sessionStartDate = start, sessionExpireDate = end }
             logInfo $ [s|new session. sessionId=%s|] sid
             Kvs.setWithTtl SessionKvs sid sd ttl
             return $ SessionState sid sd isSecure
    where
    sname = sessionName sessionSettings
    len = sessionIdLength sessionSettings
    ttl = sessionTtl sessionSettings
    isSecure = sessionIsSecure sessionSettings



loadSession
    ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , SetMember Lift (Lift IO) r
    )
    => T.Time
    -> Maybe B.ByteString
    -> Eff r SessionState
loadSession current requestSessionId = do
    sd <- maybe (return Nothing) (Kvs.get SessionKvs) requestSessionId
    maybe (return defaultSessionState) deleteIfExpired ((,) <$> requestSessionId <*> sd)

    where
    deleteIfExpired (sid, sd)
        | current < sessionExpireDate sd = do
            logDebug $ [s|load session. sessionId=%s sessionData=%s|] sid (show sd)
            return SessionState { sessionId = sid, sessionData = sd, isNew = False }
        | otherwise = do
            Kvs.delete SessionKvs sid
            return defaultSessionState



saveSession
     ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , SetMember Lift (Lift IO) r
    )
    => T.Time
    -> SessionState
    -> Eff r ()
saveSession current (SessionState sid sd _) =
    when (sid /= "") $ do
        let expire = sessionExpireDate sd
            ttl' = T.diffTime expire current
        when (ttl' > 0) $ do
            Kvs.setWithTtl SessionKvs sid sd ttl'
            logDebug $ [s|save session. session=%s ttl=%d|] (show sd) ttl'



destroySession
     ::
    ( Member Exception r
    , Member Logger r
    , Member (Kvs.Kvs SessionKvs) r
    , SetMember Lift (Lift IO) r
    )
    => B.ByteString
    -> Eff r ()
destroySession = void . Kvs.delete SessionKvs
