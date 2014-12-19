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
import qualified Data.Binary as Bin (Binary, encode, decodeOrFail)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Network.HTTP.Types as HTTP (Header)

data Session a =
    forall b. SessionGet (L.ByteString -> Maybe b) B.ByteString (Maybe b -> a) |
    forall b. SessionPut (b -> L.ByteString) B.ByteString b a |
    SessionTtl Integer a |
    SessionDestroy a |
    GetSessionId (B.ByteString -> a) |
    RenderSetCookie (HTTP.Header -> a)
    deriving Typeable

instance Functor Session where
    fmap f (SessionGet d k c) = SessionGet d k (f . c)
    fmap f (SessionPut e k v c) = SessionPut e k v (f c)
    fmap f (SessionTtl ttl c) = SessionTtl ttl (f c)
    fmap f (SessionDestroy c) = SessionDestroy (f c)
    fmap f (GetSessionId c) = GetSessionId (f . c)
    fmap f (RenderSetCookie c) = RenderSetCookie (f . c)


sget :: (Member Session r, Bin.Binary a) => B.ByteString -> Eff r (Maybe a)
sget k = send . inj . SessionGet decode k $ id
    where
    decode = either (const Nothing) (\(_,_,a) -> Just a) . Bin.decodeOrFail

sput :: (Member Session r, Bin.Binary a) => B.ByteString -> a -> Eff r ()
sput k v = send . inj . SessionPut Bin.encode k v $ ()

sttl :: (Member Session r) => Integer -> Eff r ()
sttl ttl = send . inj . SessionTtl ttl $ ()

sdestroy :: (Member Session r) => Eff r ()
sdestroy = send . inj . SessionDestroy $ ()

getSessionId :: (Member Session r) => Eff r B.ByteString
getSessionId = send . inj . GetSessionId $ id

renderSetCookie :: (Member Session r) => Eff r HTTP.Header
renderSetCookie = send . inj . RenderSetCookie $ id
