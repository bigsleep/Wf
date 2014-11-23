{-# LANGUAGE FlexibleContexts, DeriveDataTypeable, ExistentialQuantification #-}
module Wf.Control.Eff.Session
( sget
, sput
, sttl
, sdestroy
, getSessionId
, renderSetCookie
, Session(..)
) where

import Control.Eff (Eff, Member, inj, send)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import qualified Data.Aeson as DA (FromJSON, ToJSON)
import qualified Network.HTTP.Types as HTTP (Header)

data Session a =
    forall b. (DA.FromJSON b) => SessionGet B.ByteString (Maybe b -> a) |
    forall b. (DA.ToJSON b) => SessionPut B.ByteString b a |
    SessionTtl Integer a |
    SessionDestroy a |
    GetSessionId (B.ByteString -> a) |
    RenderSetCookie (HTTP.Header -> a)
    deriving (Typeable)

instance Functor Session where
    fmap f (SessionGet k c) = SessionGet k (f . c)
    fmap f (SessionPut k v c) = SessionPut k v (f c)
    fmap f (SessionTtl ttl c) = SessionTtl ttl (f c)
    fmap f (SessionDestroy c) = SessionDestroy (f c)
    fmap f (GetSessionId c) = GetSessionId (f . c)
    fmap f (RenderSetCookie c) = RenderSetCookie (f . c)

sget :: (Member Session r, DA.FromJSON a) => B.ByteString -> Eff r (Maybe a)
sget k = send $ inj . SessionGet k

sput :: (Member Session r, DA.ToJSON a) => B.ByteString -> a -> Eff r ()
sput k v = send $ \f -> inj . SessionPut k v $ f ()

sttl :: (Member Session r) => Integer -> Eff r ()
sttl ttl = send $ \f -> inj . SessionTtl ttl $ f ()

sdestroy :: (Member Session r) => Eff r ()
sdestroy = send $ \f -> inj . SessionDestroy $ f ()

getSessionId :: (Member Session r) => Eff r B.ByteString
getSessionId = send $ inj . GetSessionId

renderSetCookie :: (Member Session r) => Eff r HTTP.Header
renderSetCookie = send $ inj . RenderSetCookie
