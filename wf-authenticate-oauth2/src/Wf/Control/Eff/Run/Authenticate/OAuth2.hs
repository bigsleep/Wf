{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, TypeFamilies #-}
module Wf.Control.Eff.Run.Authenticate.OAuth2
( runAuthenticateOAuth2
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift)
import Control.Eff.Reader.Strict (Reader)
import Wf.Control.Eff.Authenticate (Authenticate(..), AuthenticationType(..))
import Wf.Control.Eff.HttpClient (HttpClient)
import Wf.Control.Eff.Session (Session)
import Wf.Web.Authenticate.OAuth2 (OAuth2(..), OAuth2Error(..), redirectToAuthorizationServer, getAccessToken, getUserInfo)
import Wf.Network.Http.Request (queryParam)
import Data.Typeable (Typeable)
import qualified Network.Wai as Wai (Request)

import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger)

runAuthenticateOAuth2
    :: ( Typeable auth
       , Member Exception r
       , Member Logger r
       , Member HttpClient r
       , Member (Reader Wai.Request) r
       , Member Session r
       , SetMember Lift (Lift IO) r
       , AuthenticationUserType auth ~ u
       )
    => OAuth2 u -> Eff (Authenticate auth :> r) a -> Eff r a
runAuthenticateOAuth2 oauth2 = loop . admin
    where
    loop (Val a) = return a

    loop (E u) = handleRelay u loop handle

    handle (Authenticate _ _ c) = do
        maybeCode <- queryParam "code"
        maybeState <- queryParam "state"
        case (maybeCode, maybeState) of
             (Just code, Just state) -> getAccessToken oauth2 code state >>= getUserInfo oauth2 >>= loop . c
             _ -> throwException . OAuth2Error $ "no code or no state in request query string"

    handle (AuthenticationTransfer _ res c) = redirectToAuthorizationServer oauth2 res >>= loop . c
