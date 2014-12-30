{-# LANGUAGE OverloadedStrings, FlexibleContexts, TypeOperators #-}
module Main where

import Control.Monad (when)
import Control.Eff (Member, Eff, (:>))
import Control.Eff.Reader.Strict (Reader, runReader)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Exception (SomeException)

import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)
import Wf.Kvs.Redis (Kvs, runKvsRedis)
import Wf.Session.Kvs (Session, sget, sput, renderSetCookie, SessionKvs, SessionSettings(..), defaultSessionSettings, runSessionKvs)
import Wf.Network.Http.Response (setStatus, addHeader, html, defaultResponse)
import Wf.Network.Wai (toWaiResponse)
import Wf.Application.Time (getCurrentTime)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger, logDebug)

import qualified Network.Wai as Wai (Request, Response, requestHeaders, requestMethod)
import qualified Network.HTTP.Types as HTTP (status500, methodPost)
import qualified Database.Redis as Redis (ConnectInfo(..), defaultConnectInfo)
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
        when (Wai.requestMethod req == HTTP.methodPost) $ sput "count" (B.pack . show $ (c + 1 :: Integer))
        let body = "<!DOCTYPE html><meta charset=\"utf-8\"><h1>" `L.append` (L.pack . show $ c) `L.append` "</h1>" `L.append` button
        return . toWaiResponse . html body $ defaultResponse ()

    button = "<form action=\"\" method=\"post\"><input type=\"submit\" value=\"++\"></form>"



main :: IO ()
main = Warp.run 8080 server
    where
    errorResponse = toWaiResponse . setStatus HTTP.status500 $ defaultResponse ()
    server request respond = either (const (respond errorResponse)) respond =<< run request app

redisConnectInfo :: Redis.ConnectInfo
redisConnectInfo = Redis.defaultConnectInfo { Redis.connectDatabase = 2 }

run ::
    Wai.Request ->
    ( Wai.Request ->
      Eff (  Session
          :> Reader Wai.Request
          :> Kvs SessionKvs
          :> Exception
          :> Logger
          :> Lift IO
          :> ()) Wai.Response) ->
    IO (Either SomeException Wai.Response)
run request a = do
    t <- getCurrentTime
    let ssettings = defaultSessionSettings { sessionName = sname, sessionTtl = 300 }
    runLift
        . runLoggerStdIO DEBUG
        . runExc
        . runKvsRedis redisConnectInfo
        . flip runReader request
        . runSessionKvs ssettings t $ a request
