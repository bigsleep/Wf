{-# LANGUAGE OverloadedStrings, FlexibleContexts, DeriveDataTypeable, TypeFamilies, TemplateHaskell #-}
module Wf.Web.Authenticate.OAuth2
( OAuth2Config(..)
, OAuth2(..)
, OAuth2Error(..)
, AccessTokenType(..)
, redirectToAuthorizationServer
, getAccessToken
, getUserInfo
) where

import Control.Eff (Eff, Member, SetMember)
import Control.Eff.Lift (Lift, lift)
import Wf.Control.Eff.HttpClient (HttpClient, httpClient)
import qualified Wf.Control.Eff.Session as Session (Session, sput, sget, sttl, renderSetCookie)
import qualified Control.Exception (Exception)
import Control.Monad (unless)

import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Char8 as B (pack, unpack)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.Text.Encoding as T (encodeUtf8)
import Data.Maybe (isJust)
import Data.Typeable (Typeable)
import qualified Data.Aeson as DA (Value(..), Result(..), decode, (.:?))
import qualified Data.Aeson.Types as DA (parse)
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)

import qualified Network.HTTP.Client as N (Request(..), RequestBody(..), Response(..), parseUrl, urlEncodedBody)
import qualified Network.HTTP.Types as HTTP (methodGet, methodPost, renderQuery, status200, hAccept, hAuthorization)

import System.Random (newStdGen, randomRs)

import Wf.Network.Http.Response (Response, addHeader, redirect)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, logDebug)

data AccessTokenType = BEARER | MAC deriving (Show, Eq)

DA.deriveJSON DA.defaultOptions ''AccessTokenType

data OAuth2Config = OAuth2Config
    { oauth2AuthorizationServerName :: B.ByteString
    , oauth2AuthorizationUri :: B.ByteString
    , oauth2TokenUri :: B.ByteString
    , oauth2UserInfoUri :: B.ByteString
    , oauth2ClientId :: B.ByteString
    , oauth2ClientSecret :: B.ByteString
    , oauth2RedirectUri :: B.ByteString
    , oauth2Scope :: B.ByteString
    , oauth2AccessTokenType :: AccessTokenType
    } deriving (Typeable, Show, Eq)

DA.deriveJSON DA.defaultOptions ''OAuth2Config

data OAuth2 user = OAuth2
    { oauth2Config :: OAuth2Config
    , oauth2UserParser :: L.ByteString -> Maybe user
    } deriving (Typeable)


data OAuth2Error = OAuth2Error String deriving (Show, Typeable)

instance Control.Exception.Exception OAuth2Error


redirectToAuthorizationServer
    :: ( Member Session.Session r
       , SetMember Lift (Lift IO) r
       )
    => OAuth2 u -> Response body -> Eff r (Response body)
redirectToAuthorizationServer oauth2 response = do
    g <- lift newStdGen
    let state = B.pack . take stateLen . map (chars !!) . randomRs (0, length chars - 1) $ g
    Session.sput "state" state
    Session.sttl 300
    setCookie <- Session.renderSetCookie
    let params = [ ("client_id", Just . oauth2ClientId . oauth2Config $ oauth2)
                 , ("response_type", Just "code")
                 , ("scope", Just . oauth2Scope . oauth2Config $ oauth2)
                 , ("redirect_uri", Just . oauth2RedirectUri . oauth2Config $ oauth2)
                 , ("state", Just state)
                 ]
        url = oauth2AuthorizationUri (oauth2Config oauth2) `B.append` HTTP.renderQuery True params
    return . redirect url . addHeader setCookie $ response
    where
    stateLen = 40
    chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']


getAccessToken
    :: (Member HttpClient r, Member Exception r, Member Session.Session r, Member Logger r)
    => OAuth2 u -> B.ByteString -> B.ByteString -> Eff r B.ByteString
getAccessToken oauth2 code state = do
    let params = [ ("code", code)
                 , ("redirect_uri", oauth2RedirectUri . oauth2Config $ oauth2)
                 , ("client_id", oauth2ClientId . oauth2Config $ oauth2)
                 , ("client_secret", oauth2ClientSecret . oauth2Config $ oauth2)
                 , ("grant_type", "authorization_code")
                 ]
        headers = [ ("Content-Type", "application/x-www-form-urlencoded")
                  , ("Accept", "application/json")
                  ]

    reqInit <- case N.parseUrl . B.unpack . oauth2TokenUri . oauth2Config $ oauth2 of
                    Right r -> return r
                    Left e -> throwException e

    let req = N.urlEncodedBody params $
              reqInit { N.method = HTTP.methodPost
                      , N.secure = True
                      , N.requestHeaders = headers
                      }

    logDebug $ "getAccessToken request: " ++ show req
    logDebug $ "getAccessToken request body: " ++ showBody (N.requestBody req)

    res <- httpClient req
    logDebug $ "getAccessToken response: " ++ show res

    unless (N.responseStatus res == HTTP.status200) .
        throwException . OAuth2Error $ "authorization server returned other than status200. response: " ++ show res

    json <- case DA.decode $ N.responseBody res of
                 Just a -> return a
                 Nothing -> throwException . OAuth2Error $ "json parse failed. response: " ++ show res

    state' <- Session.sget "state"
    unless (isJust state' && state' == Just state) .
        throwException . OAuth2Error $ "state token check failed. response: " ++ show res ++ " state: " ++ show (Just state) ++ " /= " ++ show state'
    g json "access_token"

    where g obj key = case DA.parse (DA..:? key) obj of
                           DA.Success (Just (DA.String v)) -> return (T.encodeUtf8 v)
                           _ -> throwException . OAuth2Error $ "no field. obj: " ++ show obj ++ " key: " ++ show key


getUserInfo
    :: (Member HttpClient r, Member Exception r, Member Logger r)
    => OAuth2 user -> B.ByteString -> Eff r user
getUserInfo oauth2 accessToken = do
    let headers = [ (HTTP.hAuthorization, "Bearer " `B.append` accessToken)
                  , (HTTP.hAccept, "application/json")
                  ]

    reqInit <- case N.parseUrl . B.unpack . oauth2UserInfoUri . oauth2Config $ oauth2 of
                    Right r -> return r
                    Left e -> throwException e

    let req = reqInit { N.method = HTTP.methodGet
                      , N.secure = True
                      , N.requestHeaders = headers
                      }

    logDebug $ "getUserInfo requet: " ++ show req
    res <- httpClient req
    logDebug $ "getUserInfo response: " ++ show res

    unless (N.responseStatus res == HTTP.status200) .
        throwException . OAuth2Error $ "token server returned other than status200. response: " ++ show res

    case oauth2UserParser oauth2 . N.responseBody $ res of
         Just a -> return a
         Nothing -> throwException . OAuth2Error $ "parse failed. response: " ++ show res


showBody :: N.RequestBody -> String
showBody (N.RequestBodyLBS b) = show b
showBody (N.RequestBodyBS b) = show b
showBody (N.RequestBodyBuilder _ _) = "RequestBodyBuilder"
showBody (N.RequestBodyStream _ _) = "RequestBodyStream"
showBody (N.RequestBodyStreamChunked _) = "ReuqestBodyStreamChunked"
