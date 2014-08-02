{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts #-}
module Wf.Web.Authenticate.OAuth2Spec
( oauth2Spec
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Wf.Control.Eff.HttpClient (HttpClient(..), httpClient, runHttpClientMock)
import Wf.Control.Eff.Session (Session(..), sget, sput, sttl, sdestroy, getSessionId)
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs(..), get)
import Control.Eff.Lift (Lift, runLift)
import Control.Eff.Exception (Exc, runExc, throwExc)
import Control.Eff.State.Strict (State, get, put, evalState, runState)
import Control.Eff.Reader.Strict (Reader, runReader)
import Wf.Control.Eff.Logger (LogLevel(..), runLoggerStdIO)

import Control.Exception (SomeException)

import qualified Data.List as L (lookup, head)
import Data.Maybe (listToMaybe)
import qualified Data.Map as M (Map, fromList, empty, member, null, elems)
import qualified Data.HashMap.Strict as HM (fromList, lookup)
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack, break)
import qualified Data.ByteString.Lazy as L (ByteString, fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as L (pack)
import qualified Data.Text.Encoding as T (decodeUtf8)
import qualified Data.Aeson as DA (Value(..), encode, decode)
import Data.Either (isLeft)
import Wf.Data.Serializable (serialize, deserialize)
import GHC.Exts (sortWith)

import qualified Network.Wai as Wai (Request, Response, defaultRequest, requestHeaders, responseLBS, responseStatus, responseHeaders)
import Web.Cookie (SetCookie, renderCookies)
import qualified Network.HTTP.Client.Internal as N (Request(..), Response(..), RequestBody(..))
import qualified Network.HTTP.Types as HTTP (status200, status302, status403, status501, http10, parseQuery, parseSimpleQuery, hCookie)
import Blaze.ByteString.Builder (toByteString)
import Wf.Web.Authenticate.OAuth2

import Wf.Application.Logger (Logger, logDebug)
import Wf.Application.Exception (Exception(..))
import Wf.Application.Time (Time, getCurrentTime, addSeconds, mjd)
import Wf.Control.Eff.Run.Kvs.Map (runKvsMap)
import Wf.Web.Session (SessionKvs(..), SessionError(..), SessionState(..), SessionData(..), defaultSessionState, defaultSessionData, getRequestSessionId)
import Wf.Control.Eff.Run.Session (runSession)
import Wf.Network.Http.Types (Response(..))

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import qualified Test.Hspec.QuickCheck as Q
import qualified Test.QuickCheck.Property as Q

oauth2TestSetting :: OAuth2 u m
oauth2TestSetting = OAuth2
    { oauth2AuthorizationServerName = "test"
    , oauth2AuthorizationUri = "https://test.com/authorization"
    , oauth2TokenUri = "https://test.com/token"
    , oauth2ClientId = "test_client_id"
    , oauth2ClientSecret = "test_client_secret"
    , oauth2RedirectUri = "https://test.client.com/redirect"
    , oauth2Scope = "openid email"
    }

defaultClientResponse :: N.Response L.ByteString
defaultClientResponse = N.Response {}

defaultServerResponse :: Wai.Response
defaultServerResponse = Wai.responseLBS HTTP.status501 [] ""


oauth2Spec :: Spec
oauth2Spec = describe "oauth2" $ do
    redirectAuthServerSpec
    getAccessTokenSpec


redirectAuthServerSpec :: Spec
redirectAuthServerSpec =
    it "redirect to authorization server" $ do
        let oauth2 = oauth2TestSetting
            code = redirectToAuthorizationServer oauth2 Response {}
        t <- getCurrentTime

        Right (m, res) <- runTest "SID" M.empty Wai.defaultRequest (const defaultClientResponse) t code

        let Just (redirectUri, paramsStr) = L.lookup "Location" (responseHeaders res) >>= return . B.break (== '?')
        let params = HTTP.parseQuery paramsStr
        let Just session = deserialize . L.head . M.elems $ m

        responseStatus res `shouldBe` HTTP.status302
        redirectUri `shouldBe` oauth2AuthorizationUri oauth2

        let f name ps = L.lookup name ps >>= id
        f "client_id" params `shouldBe` Just (oauth2ClientId oauth2)
        f "response_type" params `shouldBe` Just "code"
        f "scope" params `shouldBe` Just (oauth2Scope oauth2)
        f "redirect_uri" params `shouldBe` Just (oauth2RedirectUri oauth2)
        f "state" params `shouldBe` (listToMaybe =<< DA.decode =<< HM.lookup "state" (sessionValue session))


getAccessTokenSpec :: Spec
getAccessTokenSpec = describe "get access token" $ do
    let stateToken = "test_state_token_000"
        sd t = SessionData (HM.fromList [("state", DA.encode $ [stateToken])]) t t
        sname = "SID"
        sid = "test_sid"
        sessionStore t = M.fromList [(sid, serialize $ sd t)]
        cookie = (HTTP.hCookie, sname `B.append` "=" `B.append` sid)
        request = Wai.defaultRequest { Wai.requestHeaders = [cookie] }
        accessToken = "test_access_token_111"
        oauth2 = oauth2TestSetting
        code = "code_aaa"
        server = authServer oauth2 code accessToken
        run t t' req = runTest sname (sessionStore t) req server t'

    it "success with a right code and a state token" $ do
        t <-  getCurrentTime
        Right (_, result) <- run (addSeconds t 1) t request $ getAccessToken oauth2 code stateToken
        result `shouldBe` accessToken

    it "fail with wrong code" $ do
        t <- getCurrentTime
        r <- run (addSeconds t 1) t request $ getAccessToken oauth2 "wrong_code" stateToken
        shouldSatisfy r isLeft

    it "fail with wrong stateToken" $ do
        t <- getCurrentTime
        r <- run (addSeconds t 1) t request $ getAccessToken oauth2 code "wrong_state"
        shouldSatisfy r isLeft

    it "fail if session expired" $ do
        t <- getCurrentTime
        r <- run t t request $ getAccessToken oauth2 code stateToken
        shouldSatisfy r isLeft

    it "fail without cookie" $ do
        t <- getCurrentTime
        r <- run (addSeconds t 1) t Wai.defaultRequest $ getAccessToken oauth2 code "wrong_state"
        shouldSatisfy r isLeft


runTest ::
    B.ByteString ->
    M.Map B.ByteString L.ByteString ->
    Wai.Request ->
    (N.Request -> N.Response L.ByteString) ->
    Time ->
    Eff ( HttpClient
        :> Session
        :> Kvs.Kvs SessionKvs
        :> State (M.Map B.ByteString L.ByteString)
        :> State SessionState
        :> Exception
        :> Logger
        :> Lift IO
        :> ()) a ->
    IO (Either SomeException (M.Map B.ByteString L.ByteString, a))
runTest name s request cresponse t =
      runLift
    . runLoggerStdIO DEBUG
    . runExc
    . evalState defaultSessionState
    . runState s
    . runKvsMap
    . runSession name t (getRequestSessionId name . Wai.requestHeaders $ request) False 0
    . runHttpClientMock cresponse


authServer :: OAuth2 u m -> B.ByteString -> B.ByteString -> N.Request -> N.Response L.ByteString
authServer oauth2 code accessToken request =
    if params == expected
        then successResponse
        else errorResponse
    where expected = sortWith fst
                   [ ("code", code)
                   , ("client_id", oauth2ClientId oauth2)
                   , ("client_secret", oauth2ClientSecret oauth2)
                   , ("redirect_uri", oauth2RedirectUri oauth2)
                   , ("grant_type", "authorization_code")
                   ]
          params = case N.requestBody request of
                        N.RequestBodyBS body -> sortWith fst . HTTP.parseSimpleQuery $ body
                        _ -> []
          successResponse = defaultClientResponse
                          { N.responseStatus = HTTP.status200
                          , N.responseBody = DA.encode . M.fromList $ [("access_token" :: B.ByteString, accessToken)]
                          }
          errorResponse = defaultClientResponse { N.responseStatus = HTTP.status403 }


tokenInfoServer :: OAuth2 u m -> B.ByteString -> DA.Value -> N.Request -> N.Response L.ByteString
tokenInfoServer oauth2 accessToken tokenInfo request =
    if requestAccessToken == Just ("OAuth " `B.append` accessToken)
        then successResponse
        else errorResponse
    where requestAccessToken = L.lookup "Authorization" $ N.requestHeaders request
          successResponse = defaultClientResponse
                          { N.responseStatus = HTTP.status200
                          , N.responseBody = DA.encode tokenInfo
                          }
          errorResponse = defaultClientResponse { N.responseStatus = HTTP.status403 }

