{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Wf.Control.Eff.Run.SessionSpec
( sessionSpec
) where

import Control.Eff (Eff, (:>), Member, SetMember, handleRelay)
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs(..), get)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.State.Strict (State, get, put, evalState, runState)
import Control.Eff.Reader.Strict (Reader, runReader)
import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)

import Control.Exception (SomeException)

import qualified Data.Binary as Bin (encode)
import qualified Data.Map as M (Map, fromList, empty, member, null)
import qualified Data.HashMap.Strict as HM (fromList)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)

import Web.Cookie (SetCookie, renderCookies)
import Blaze.ByteString.Builder (toByteString)

import Wf.Application.Logger (Logger, logDebug)
import Wf.Control.Eff.Run.Kvs.Map (runKvsMap)
import Wf.Application.Exception (Exception(..))
import Wf.Session.Kvs (Session(..), defaultSessionSettings, sget, sput, sttl, sdestroy, getSessionId, SessionKvs(..), SessionError(..), SessionState(..), SessionData(..), SessionSettings(..), defaultSessionState, defaultSessionData, runSessionKvs)
import Wf.Control.Eff.Run.Session (runSession)
import Wf.Application.Time (Time, getCurrentTime, addSeconds)
import qualified Network.Wai as Wai (Request(..), defaultRequest)

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

sessionSpec :: Spec
sessionSpec = describe "session" $ do
    it "start automatically" $ do
        t <- getCurrentTime
        let key = "state" :: B.ByteString
        let val = "hello" :: B.ByteString
        let code = do
                    sput key val
                    sttl 10
                    getSessionId
        Right (s, sid) <- runTest "SID" t Wai.defaultRequest M.empty code
        shouldSatisfy s (M.member sid)

    it "restore automatically" $ do
        let name = "SID"
        let sid = "testSessionId000"
        let cookies = [(name, sid)]
        let headers = [("Cookie", toByteString . renderCookies $ cookies)]
        let request = Wai.defaultRequest { Wai.requestHeaders = headers }
        let key = "hello"
        let val = ("world", 1, [3]) :: (String, Integer, [Integer])
        let sval = HM.fromList [(key, Bin.encode val)]
        t <- getCurrentTime
        let expireDate = addSeconds t 10
        let sd = Bin.encode SessionData { sessionValue = sval, sessionStartDate = t, sessionExpireDate = expireDate }
        let sessionState = M.fromList [(sid, sd)]
        let code = do
                    v <- sget key
                    sid' <- getSessionId
                    return (v, sid')
        Right (_, result) <- runTest name t request sessionState code
        result `shouldBe` (Just val, sid)

    it "destroy" $ do
        t <- getCurrentTime
        let key = "state" :: B.ByteString
        let val = "hello" :: B.ByteString
        let code = do
                    sput key val
                    before <- getSessionId
                    sdestroy
                    after <- getSessionId
                    return (before, after)
        Right (s, (_, afterId)) <- runTest "SID" t Wai.defaultRequest M.empty code
        afterId `shouldBe` ""
        shouldSatisfy s M.null



runTest :: B.ByteString ->
           Time ->
           Wai.Request ->
           M.Map B.ByteString L.ByteString ->
           Eff ( Session
              :> Reader Wai.Request
              :> Kvs.Kvs SessionKvs
              :> State (M.Map B.ByteString L.ByteString)
              :> Exception
              :> Logger
              :> Lift IO
              :> ()) a ->
           IO (Either SomeException (M.Map B.ByteString L.ByteString, a))
runTest name t request s a = do
    let ssettings = defaultSessionSettings { sessionName = name }
    runLift
        . runLoggerStdIO DEBUG
        . runExc
        . runState s
        . runKvsMap
        . flip runReader request
        . runSessionKvs ssettings t $ a
