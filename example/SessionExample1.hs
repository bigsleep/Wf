{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module Main where

import Control.Eff (Member, Eff, (:>))
import Control.Eff.State.Strict (State, evalState)
import Control.Eff.Reader.Strict (Reader, ask, runReader)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Exception (SomeException)

import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)
import Wf.Kvs.Redis (Kvs, runKvsRedis)
import Wf.Web.Session (Session, sget, sput, renderSetCookie, SessionState, defaultSessionState, SessionKvs, getRequestSessionId, SessionSettings(..))
import Wf.Control.Eff.Run.Session.Kvs (runSessionKvs)
import Wf.Network.Http.Response (setStatus, addHeader, html, defaultResponse)
import Wf.Network.Wai (toWaiResponse)
import Wf.Application.Time (Time, getCurrentTime)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug)

import qualified Network.Wai as Wai (Request, Response, defaultRequest, requestHeaders, responseLBS, responseStatus, responseHeaders)
import qualified Network.HTTP.Types as HTTP (status200, status500)
import qualified Database.Redis as Redis (ConnectInfo(..), defaultConnectInfo, PortID(..))
import qualified Network.Wai.Handler.Warp as Warp (run)

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (append)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.ByteString.Char8 as B (pack, unpack)

sname :: B.ByteString
sname = "SID"

app :: (Member Session r, Member Logger r) => Wai.Request -> Eff r Wai.Response
app req = do
    logDebug . show $ Wai.requestHeaders req
    count <- sget "count"
    exec $ fmap (read . B.unpack) count

    where
    exec Nothing = do
        sput "count" (B.pack . show $ (1 :: Integer))
        setCookie <- renderSetCookie
        let body = "<!DOCTYPE html><h1>0</h1>" `L.append` button
        return . toWaiResponse . addHeader setCookie . html body $ defaultResponse ()

    exec (Just c) = do
        sput "count" (B.pack . show $ (c + 1 :: Integer))
        let body = "<!DOCTYPE html><h1>" `L.append` (L.pack . show $ c) `L.append` "</h1>" `L.append` button
        return . toWaiResponse . html body $ defaultResponse ()

    button = "<form action=\"\" method=\"get\"><input type=\"submit\" value=\"++\"></form>"



main :: IO ()
main = do
    Warp.run 8080 server
    where
    errorResponse = toWaiResponse . setStatus HTTP.status500 $ defaultResponse ()
    server request respond = do
        let requestSessionId = getRequestSessionId sname . Wai.requestHeaders $ request
        r <- run (app request) requestSessionId
        case r of
             Right res -> respond res
             Left _ -> respond errorResponse

redisConnectInfo :: Redis.ConnectInfo
redisConnectInfo = Redis.defaultConnectInfo { Redis.connectDatabase = 2 }

run ::
    Eff (  Session
        :> Kvs SessionKvs
        :> State SessionState
        :> Exception
        :> Logger
        :> Lift IO
        :> ()) Wai.Response ->
    Maybe B.ByteString ->
    IO (Either SomeException Wai.Response)
run eff requestSessionId = do
    t <- getCurrentTime
    let ssettings = SessionSettings sname False 300 40
    runLift
        . runLoggerStdIO DEBUG
        . runExc
        . evalState defaultSessionState
        . runKvsRedis redisConnectInfo
        . runSessionKvs ssettings t requestSessionId $ eff
