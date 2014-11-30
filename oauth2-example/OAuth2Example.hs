{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, TemplateHaskell, TypeFamilies, DeriveGeneric #-}
module Main where

import Control.Eff (Eff, (:>))
import Control.Eff.Reader.Strict (Reader, runReader)
import Control.Eff.Exception (runExc)
import Control.Eff.Lift (Lift, runLift, lift)
import Control.Monad (mzero)
import Control.Exception (SomeException(..))

import qualified Data.List as List (lookup)
import Data.Typeable (cast)
import qualified Data.Binary as Bin (Binary(..))
import qualified Data.ByteString as B (ByteString, append)
import qualified Data.ByteString.Lazy as L (ByteString, readFile)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), decode, encode, (.:))
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import GHC.Generics (Generic)

import qualified Network.Wai as Wai (Request, Response)
import qualified Network.HTTP.Client as N (Manager, newManager)
import qualified Network.HTTP.Client.TLS as N (tlsManagerSettings)
import qualified Network.HTTP.Types as HTTP (status500)
import qualified Network.Wai.Handler.Warp as Warp (run)

import Wf.Control.Eff.HttpClient (HttpClient, runHttpClient)
import Wf.Control.Eff.Authenticate (Authenticate, AuthenticationType(..), authenticate, authenticationTransfer)
import Wf.Control.Eff.Run.Authenticate.OAuth2 (runAuthenticateOAuth2)
import Wf.Web.Authenticate.OAuth2 (OAuth2(..))
import Wf.Network.Http.Types (Request, defaultResponse, requestQuery)
import Wf.Network.Http.Request (queryParam)
import Wf.Network.Http.Response (setStatus, addHeader, redirect, file, json)
import Wf.Network.Wai (fromWaiRequest, toWaiResponse, toWaiApplication)
import Wf.Session.Stm (Session, sget, sput, sdestroy, renderSetCookie, initializeSessionStore, SessionSettings, SessionStore, runSessionStm)
import Wf.Web.Api (apiRoutes, getApi, postApi)
import Wf.Application.Time (getCurrentTime)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, logDebug, runLoggerStdIO, LogLevel(..))

import Settings (Settings(..))

data User = User
    { userId :: B.ByteString
    , userName :: B.ByteString
    , userEmail :: B.ByteString
    } deriving (Show, Eq, Generic)

DA.deriveJSON DA.defaultOptions ''User

instance Bin.Binary User

newtype GoogleUser = GoogleUser { unGoogleUser :: User } deriving (Show, Eq)

instance DA.FromJSON GoogleUser where
    parseJSON (DA.Object o) = do
        uid <- o DA..: "id"
        uname <- o DA..: "displayName"
        uemail' <- fmap head (o DA..: "emails")
        uemail <- uemail' DA..: "value"
        return $ GoogleUser User { userId = uid, userName = uname, userEmail = uemail }
    parseJSON _ = mzero

main :: IO ()
main = do
    settings <- loadSettings
    manager <- N.newManager N.tlsManagerSettings
    sessionStore <- initializeSessionStore (2 *60 * 1000000)

    let oauth2 = OAuth2 { oauth2Config = settingsOAuth2 settings, oauth2UserParser = fmap unGoogleUser . DA.decode }
        port = settingsPort settings
        uri = settingsUri settings
        sessionSettings = settingsSession settings
        server = toWaiApplication $ run oauth2 manager sessionStore sessionSettings (routes uri)

    Warp.run port server

    where
    loadSettings :: IO Settings
    loadSettings = do
        a <- fmap DA.decode . L.readFile $ "config/settings.json"
        case a of
             Just settings -> return settings
             Nothing -> error "load failure"


type M = Eff
    (  Authenticate ()
    :> HttpClient
    :> Session
    :> Reader Wai.Request
    :> Logger
    :> Exception
    :> Lift IO
    :> ())

routes :: B.ByteString -> Wai.Request -> M Wai.Response
routes uri = apiRoutes rootApp rs
    where
    rs = [ getApi "/" (const rootApp :: Wai.Request -> M Wai.Response)
         , postApi "/login" loginApp
         , getApi "/oauth2callback" (oauth2CallbackApp uri)
         ]

instance AuthenticationType () where
    type AuthenticationKeyType () = ()
    type AuthenticationUserType () = User

rootApp :: M Wai.Response
rootApp = do
    maybeUser <- sget "login_user" :: M (Maybe User)

    case maybeUser of
         Just user -> return . toWaiResponse . json (DA.encode user) $ defaultResponse ()
         Nothing -> return . toWaiResponse . file "static/index.html" $ defaultResponse ()

loginApp :: Wai.Request -> M Wai.Response
loginApp _ = fmap toWaiResponse $ authenticationTransfer () $ defaultResponse ()

oauth2CallbackApp :: B.ByteString -> Wai.Request -> M Wai.Response
oauth2CallbackApp uri req = do
    req' <- lift (fromWaiRequest req :: IO (Request L.ByteString))
    user <- authenticate () ()

    logDebug $ "loginUser: " ++ show user

    sdestroy
    sput "login_user" user
    setCookie <- renderSetCookie
    return . toWaiResponse . redirect uri . addHeader setCookie $ defaultResponse ()


run :: OAuth2 User
    -> N.Manager
    -> SessionStore
    -> SessionSettings
    -> (Wai.Request -> M Wai.Response)
    -> Wai.Request
    -> IO Wai.Response
run oauth2 manager sessionStore sessionSettings app request = do
    t <- getCurrentTime
    run' t request

    where
    run' t =
          runLift
        . (>>= handleError)
        . runExc
        . runLoggerStdIO DEBUG
        . flip runReader request
        . runSessionStm sessionStore sessionSettings t
        . runHttpClient manager
        . runAuthenticateOAuth2 oauth2
        . app
    internalError = toWaiResponse . setStatus HTTP.status500 . file "oauth2-example/static/error.html" $ defaultResponse ()
    handleError (Left (SomeException e)) = do
        lift $ print e
        case cast e of
             Just r -> return r
             _ -> return internalError
    handleError (Right r) = return r
