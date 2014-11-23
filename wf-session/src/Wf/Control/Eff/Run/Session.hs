{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts #-}
module Wf.Control.Eff.Run.Session
( runSession
, getRequestSessionId
, genSessionId
) where

import Control.Eff (Eff, VE(..), (:>), Member, admin, handleRelay)
import qualified Control.Eff.State.Strict as State (State, get, put, modify)
import Wf.Control.Eff.Session (Session(..))
import Control.Monad (when)

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.List as L (lookup)
import qualified Data.HashMap.Strict as HM (lookup, insert)
import Data.Maybe (listToMaybe)
import qualified Blaze.ByteString.Builder as Blaze (toByteString)
import qualified Data.Aeson as DA (decode, encode)

import qualified Web.Cookie as Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookieExpires, setCookieSecure)

import System.Random (newStdGen, randomRs)

import Wf.Network.Http.Types (RequestHeader)
import Wf.Web.Session.Types (SessionState(..), SessionData(..), SessionSettings(..), SessionHandler(..), defaultSessionState)
import qualified Wf.Application.Time as T (Time, formatTime, addSeconds)

runSession
    ::
    ( Member (State.State SessionState) r
    )
    => SessionHandler (Eff r)
    -> SessionSettings
    -> T.Time
    -> Maybe B.ByteString
    -> Eff (Session :> r) a
    -> Eff r a
runSession handler sessionSettings current requestSessionId eff = do
    State.put =<< loadSession
    r <- loop . admin $ eff
    saveSession =<< State.get
    return r

    where
    sname = sessionName sessionSettings

    isSecure = sessionIsSecure sessionSettings

    loop (Val a) = return a

    loop (E u) = handleRelay u loop handle

    newSession = sessionHandlerNew handler sessionSettings current

    loadSession = sessionHandlerLoad handler current requestSessionId

    saveSession = sessionHandlerSave handler current

    sessionDestroy = sessionHandlerDestroy handler

    handle (SessionGet k c) = do
        m <- return . HM.lookup k . sessionValue . sessionData =<< State.get
        loop . c $ listToMaybe =<< DA.decode =<< m

    handle (SessionPut k v c) = do
        sd <- State.get
        when (sessionId sd == "") (State.put =<< newSession)
        State.modify f
        loop c

        where
        f ses @ (SessionState _ d @ (SessionData m _ _)  _) = ses { sessionData = d { sessionValue = HM.insert k encoded m } }
        encoded = DA.encode [v]

    handle (SessionTtl ttl' c) = do
        let expire = T.addSeconds current ttl'
        State.modify (f expire)
        loop c

        where
        f expire ses @ (SessionState _ d _) = ses { sessionData = d { sessionExpireDate = expire } }

    handle (SessionDestroy c) = do
        sid <- fmap sessionId State.get
        when (sid /= "") (sessionDestroy sid)
        State.put defaultSessionState
        loop c

    handle (GetSessionId c) =
        loop . c . sessionId =<< State.get

    handle (RenderSetCookie c) = do
        session <- State.get
        let sid = sessionId session
            expire = sessionExpireDate . sessionData $ session
            setCookie = Cookie.def
                      { Cookie.setCookieName = sname
                      , Cookie.setCookieValue = sid
                      , Cookie.setCookieExpires = Just expire
                      , Cookie.setCookieSecure = isSecure
                      }
        loop . c $ ("Set-Cookie", Blaze.toByteString . Cookie.renderSetCookie $ setCookie)


getRequestSessionId :: B.ByteString -> [RequestHeader] -> Maybe B.ByteString
getRequestSessionId name = (L.lookup name =<<) . fmap Cookie.parseCookies . L.lookup "Cookie"

genRandomByteString :: Int -> IO B.ByteString
genRandomByteString len = return . B.pack . take len . map (chars !!) . randomRs (0, length chars - 1) =<< newStdGen
    where
    chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

genSessionId :: B.ByteString -> T.Time -> Int -> IO B.ByteString
genSessionId sname t len = do
    let dateStr = B.pack $ T.formatTime ":%Y%m%d:" t
    randomStr <- genRandomByteString len
    return $ sname `B.append` dateStr `B.append` randomStr
