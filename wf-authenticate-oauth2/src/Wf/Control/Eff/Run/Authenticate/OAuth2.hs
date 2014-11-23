{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, TypeFamilies #-}
module Wf.Control.Eff.Run.Authenticate.OAuth2
( runAuthenticateOAuth2
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift)
import Wf.Control.Eff.Authenticate (Authenticate(..), AuthenticationType(..))
import Wf.Control.Eff.HttpClient (HttpClient)
import Wf.Control.Eff.Session (Session)
import Wf.Web.Authenticate.OAuth2 (OAuth2(..), redirectToAuthorizationServer, getAccessToken, getUserInfo)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)

import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger)

runAuthenticateOAuth2
    :: ( Typeable auth
       , Member Exception r
       , Member Logger r
       , Member HttpClient r
       , Member Session r
       , SetMember Lift (Lift IO) r
       , AuthenticationKeyType auth ~ (B.ByteString, B.ByteString)
       , AuthenticationUserType auth ~ u
       )
    => OAuth2 u -> Eff (Authenticate auth :> r) a -> Eff r a
runAuthenticateOAuth2 oauth2 = loop . admin
    where
    loop (Val a) = return a

    loop (E u) = handleRelay u loop handle

    handle (Authenticate _ (code, state) c) = getAccessToken oauth2 code state >>= getUserInfo oauth2 >>= loop . c

    handle (AuthenticationTransfer _ res c) = redirectToAuthorizationServer oauth2 res >>= loop . c
