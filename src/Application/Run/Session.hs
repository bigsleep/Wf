{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, QuasiQuotes, QuasiQuotes, TypeFamilies #-}
module Application.Run.Session
( runSession
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Reader.Strict (Reader, ask)
import qualified Control.Eff.State.Strict as State (State, get, put, modify)
import Control.Eff.Lift (Lift, lift)
import Control.Eff.Session (Session(..))
import qualified Control.Eff.Kvs as Kvs (Kvs, get, setWithTtl, delete, exists, ttl)

import Control.Monad (when)
import Control.Applicative ((<$>), (<*>))

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.List as L (lookup)
import qualified Data.HashMap.Strict as HM (lookup, insert, empty)
import Data.Maybe (fromMaybe)

import qualified Network.Wai as Wai (Request, requestHeaders)
import qualified Web.Cookie as Cookie (parseCookies)

import System.Random (getStdGen, randomRs)
import Text.Printf.TH (s)

import Application.Session (SessionState(..), SessionData(..), SessionKvs(..), defaultSessionState)
import Application.Exception (Exception)
import Application.Logger (Logger, logDebug, logInfo, logError)
import qualified Application.Time as T (Time, formatTime, addSeconds, diffTime)

runSession :: ( Member Exception r
              , Member Logger r
              , Member (Kvs.Kvs SessionKvs) r
              , Member (Reader T.Time) r
              , Member (Reader Wai.Request) r
              , Member (State.State SessionState) r
              , SetMember Lift (Lift IO) r
              )
           => B.ByteString -> Integer -> Eff (Session :> r) a -> Eff r a
runSession sessionName ttl eff = do
    loadSession
    r <- loop . admin $ eff
    saveSession
    return r
    where loop (Val a) = return a

          loop (E u) = handleRelay u loop handle

          loadSession = do
            request <- ask
            sid <- return . getRequestSessionId sessionName $ request
            sd <- maybe (return Nothing) (Kvs.get SessionKvs) sid
            maybe (return ()) putLoadedSession ((,) <$> sid <*> sd)
            logDebug $ [s|load session. session=%s|] (show sd)

          putLoadedSession (sid, sd) = do
            cur <- ask
            if cur < sessionExpireDate sd
                then State.put SessionState { sessionId = sid, sessionData = sd, isNew = False }
                else Kvs.delete SessionKvs sid >> State.put defaultSessionState

          saveSession = do
            sd <- State.get
            let sid = sessionId sd
            when (sid /= "") $ do
                current <- ask
                let expire = sessionExpireDate . sessionData $ sd
                    ttl' = T.diffTime expire current
                when (ttl' > 0) $ do
                    Kvs.setWithTtl SessionKvs sid (sessionData sd) ttl'
                    logDebug $ [s|save session. session=%s ttl=%d|] (show sd) ttl'

          newSession = do
            t <- ask
            let len = 100
            sid <- lift $ genSessionId t len
            duplicate <- Kvs.exists SessionKvs sid
            if duplicate
                then newSession
                else do
                     let start = t
                     let end = T.addSeconds start ttl
                     let sd = SessionData HM.empty start end
                     State.put SessionState { sessionId = sid, sessionData = sd, isNew = True }
                     logInfo $ [s|new session. sessionId=%s|] sid

          handle (SessionGet k c) =
            loop . c . HM.lookup k . sessionValue . sessionData =<< State.get

          handle (SessionPut k v c) = do
            sd <- State.get
            when (sessionId sd == "") newSession
            State.modify f
            loop c
            where f ses @ (SessionState _ d @ (SessionData m _ _)  _) = ses { sessionData = d { sessionValue = HM.insert k v m } }

          handle (SessionTtl ttl' c) = do
            t <- ask
            let expire = T.addSeconds t ttl'
            State.modify (f expire)
            loop c
            where f expire ses @ (SessionState _ d _) = ses { sessionData = d { sessionExpireDate = expire } }

          handle (SessionDestroy c) = do
            sid <- fmap sessionId State.get
            when (sid /= "") (Kvs.delete SessionKvs sid >> return ())
            State.put defaultSessionState
            loop c

          handle (GetSessionId c) =
            loop . c . sessionId =<< State.get


getRequestSessionId :: B.ByteString -> Wai.Request -> Maybe B.ByteString
getRequestSessionId name = (L.lookup name =<<) . fmap Cookie.parseCookies . L.lookup "Cookie" . Wai.requestHeaders

genRandomByteString :: Int -> IO B.ByteString
genRandomByteString len = return . B.pack . take len . map (chars !!) . randomRs (0, length chars - 1) =<< getStdGen
    where chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']

genSessionId :: T.Time -> Int -> IO B.ByteString
genSessionId t len = do
    let dateStr = B.pack $ T.formatTime ":%Y%m%d:" t
    randomStr <- genRandomByteString len
    return $ "SID" `B.append` dateStr `B.append` randomStr

