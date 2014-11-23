{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes #-}
module Wf.Control.Eff.Run.Session.Stm
( initializeSessionStore
, runSessionStm
, SessionStore
) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.HashMap.Strict as HM (HashMap, lookup, insert, delete, empty, member, filter)

import Control.Concurrent.STM (STM, TVar, newTVar, readTVar, modifyTVar', atomically)
import Control.Concurrent (forkIO, threadDelay)
import Control.Eff (Eff, (:>), Member, SetMember)
import qualified Control.Eff.State.Strict as State (State)
import Control.Eff.Lift (Lift, lift)
import Control.Monad (when, void, forever)
import Control.Applicative ((<$>), (<*>))

import Wf.Control.Eff.Session (Session(..))
import Wf.Control.Eff.Run.Session (genSessionId, runSession)
import Wf.Web.Session.Types (SessionState(..), SessionData(..), SessionSettings(..), SessionHandler(..), defaultSessionState, defaultSessionData)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug, logInfo)
import qualified Wf.Application.Time as T (Time, addSeconds, diffTime, getCurrentTime)

import Text.Printf.TH (s)

data SessionStore = SessionStore (TVar (HM.HashMap B.ByteString SessionData))

initializeSessionStore :: Integer -> IO SessionStore
initializeSessionStore gcInterval = do
    tv <- atomically . newTVar $ HM.empty
    void . forkIO $ forever (gc tv)
    return (SessionStore tv)
    where
    gc tv = do
        threadDelay . fromInteger $ gcInterval
        current <- T.getCurrentTime
        atomically . modifyTVar' tv $ HM.filter (living current . sessionExpireDate)
    living current t = current < t



runSessionStm
    ::
    ( Member Exception r
    , Member Logger r
    , Member (State.State SessionState) r
    , SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> SessionSettings
    -> T.Time
    -> Maybe B.ByteString
    -> Eff (Session :> r) a
    -> Eff r a

runSessionStm ss = runSession (sessionHandlerStm ss)



sessionHandlerStm
    ::
    ( Member Exception r
    , Member Logger r
    , SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> SessionHandler (Eff r)
sessionHandlerStm ss = SessionHandler
    { sessionHandlerNew = newSession ss
    , sessionHandlerLoad = loadSession ss
    , sessionHandlerSave = saveSession ss
    , sessionHandlerDestroy = destroySession ss
    }



newSession
    ::
    ( Member Exception r
    , Member Logger r
    , SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> SessionSettings
    -> T.Time
    -> Eff r SessionState
newSession sstore@(SessionStore tv) sessionSettings current = do
    sid <- lift $ genSessionId sname current (fromInteger len)
    retryIfDuplicate =<< tryNewSession sid

    where
    sname = sessionName sessionSettings
    len = sessionIdLength sessionSettings
    ttl = sessionTtl sessionSettings
    isSecure = sessionIsSecure sessionSettings
    end = T.addSeconds current ttl
    sd = defaultSessionData { sessionStartDate = current, sessionExpireDate = end }

    tryNewSession sid = lift . atomically $ do
        duplicate <- HM.member sid <$> readTVar tv
        if duplicate
            then return Nothing
            else return . Just $ SessionState sid sd isSecure

    retryIfDuplicate Nothing = newSession sstore sessionSettings current

    retryIfDuplicate (Just ss) = do
        logInfo $ [s|new session. sessionId=%s|] (sessionId ss)
        return ss



loadSession
    ::
    ( SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> T.Time
    -> Maybe B.ByteString
    -> Eff r SessionState
loadSession (SessionStore tv) current requestSessionId =
    lift . atomically $ loadSession'

    where
    loadSession' :: STM SessionState
    loadSession' = do
        sd <- maybe (return Nothing) load requestSessionId
        maybe (return defaultSessionState) deleteIfExpired ((,) <$> requestSessionId <*> sd)

    load k = HM.lookup k <$> readTVar tv

    deleteIfExpired (sid, sd)
        | current < sessionExpireDate sd =
            return SessionState { sessionId = sid, sessionData = sd, isNew = False }
        | otherwise = do
            modifyTVar' tv $ HM.delete sid
            return defaultSessionState



saveSession
     ::
    ( Member Logger r
    , SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> T.Time
    -> SessionState
    -> Eff r ()
saveSession (SessionStore tv) current (SessionState sid sd _) =
    when (sid /= "") $ do
        let expire = sessionExpireDate sd
            ttl' = T.diffTime expire current
        when (ttl' > 0) $ do
            lift . atomically . modifyTVar' tv $ HM.insert sid sd
            logDebug $ [s|save session. session=%s ttl=%d|] (show sd) ttl'



destroySession
     ::
    ( SetMember Lift (Lift IO) r
    )
    => SessionStore
    -> B.ByteString
    -> Eff r ()
destroySession (SessionStore tv) = lift . atomically . modifyTVar' tv . HM.delete

