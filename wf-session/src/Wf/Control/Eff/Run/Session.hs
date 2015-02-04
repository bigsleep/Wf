{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, ScopedTypeVariables #-}
module Wf.Control.Eff.Run.Session
( runSession
, getRequestSessionId
, genSessionId
) where

import Control.Eff (Eff, (:>), Member, freeMap, handleRelay)
import Control.Eff.Reader.Strict (Reader, ask)
import Wf.Control.Eff.Session (Session(..))
import Control.Monad (when)

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.List as L (lookup)
import qualified Data.HashMap.Strict as HM (lookup, insert)
import qualified Blaze.ByteString.Builder as Blaze (toByteString)
import qualified Network.Wai as Wai (Request, requestHeaders, isSecure)

import qualified Web.Cookie as Cookie (parseCookies, renderSetCookie, def, setCookieName, setCookieValue, setCookieExpires, setCookieSecure, setCookieHttpOnly, setCookieDomain, setCookiePath)

import Wf.Session.Types (SessionState(..), SessionData(..), SessionSettings(..), SetCookieSettings(..), SessionHandler(..), defaultSessionState)
import qualified Wf.Application.Time as T (Time, formatTime, addSeconds)
import Wf.Application.Random (randomByteString)

runSession
    :: Member (Reader Wai.Request) r
    => SessionHandler (Eff r)
    -> SessionSettings
    -> T.Time
    -> Eff (Session :> r) a
    -> Eff r a
runSession handler sessionSettings current eff = do
    requestSessionId <- fmap (getRequestSessionId sname) ask
    s <- loadSession requestSessionId
    (r, s') <- loop s eff
    when (s' /= s) $ saveSession s'
    return r

    where
    sname = sessionName sessionSettings

    scSettings = sessionSetCookieSettings sessionSettings

    loop s = freeMap (\a -> return (a, s)) $
        \u -> handleRelay u (loop s) (handle s)

    newSession = sessionHandlerNew handler sessionSettings current

    loadSession = sessionHandlerLoad handler current

    saveSession = sessionHandlerSave handler current

    sessionDestroy = sessionHandlerDestroy handler

    handle s (SessionGet decode k c) = do
        let m = HM.lookup k . sessionValue . sessionData $ s
        loop s . c $ decode =<< m

    handle s (SessionPut encode k v c) = do
        s' <- if sessionId s == ""
                 then newSession
                 else return s
        loop (f s') c

        where
        f ses @ (SessionState _ d @ (SessionData m _ _)  _) = ses { sessionData = d { sessionValue = HM.insert k (encode v) m } }

    handle s (SessionTtl ttl' c) = loop (f s) c

        where
        expire = T.addSeconds current ttl'
        f ses @ (SessionState _ d _) = ses { sessionData = d { sessionExpireDate = expire } }

    handle s (SessionDestroy c) = do
        when (sid /= "") (sessionDestroy sid)
        loop defaultSessionState c

        where
        sid = sessionId s

    handle s (GetSessionId c) =
        loop s . c . sessionId $ s

    handle s (RenderSetCookie c) = do
        requestIsSecure <- fmap Wai.isSecure ask
        loop s . c $ ("Set-Cookie", Blaze.toByteString . Cookie.renderSetCookie $ setCookie requestIsSecure)

        where
        sid = sessionId s
        expires = if addExpires scSettings
                    then Just . sessionExpireDate . sessionData $ s
                    else Nothing
        setCookie requestIsSecure = Cookie.def
                  { Cookie.setCookieName = sname
                  , Cookie.setCookieValue = sid
                  , Cookie.setCookieExpires = expires
                  , Cookie.setCookieSecure = requestIsSecure && addSecureIfHttps scSettings
                  , Cookie.setCookieHttpOnly = isHttpOnly scSettings
                  , Cookie.setCookieDomain = cookieDomain scSettings
                  , Cookie.setCookiePath = cookiePath scSettings
                  }

getRequestSessionId :: B.ByteString -> Wai.Request -> Maybe B.ByteString
getRequestSessionId name = (L.lookup name =<<) . fmap Cookie.parseCookies . L.lookup "Cookie" . Wai.requestHeaders

genSessionId :: B.ByteString -> T.Time -> Int -> IO B.ByteString
genSessionId sname t len = do
    let dateStr = B.pack $ T.formatTime ":%Y%m%d:" t
    randomStr <- randomByteString len
    return $ sname `B.append` dateStr `B.append` randomStr
