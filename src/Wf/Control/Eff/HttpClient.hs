{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, DeriveFunctor #-}
module Wf.Control.Eff.HttpClient
( HttpClient(..)
, httpClient
, runHttpClient
, runHttpClientMock
) where

import Control.Eff ((:>), Eff, Member, SetMember, handleRelay, inj, send, freeMap)
import Control.Eff.Lift (Lift, lift)
import Data.Typeable (Typeable)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Network.HTTP.Client as N (Request(..), Response(..), Manager, httpLbs)

data HttpClient a = HttpClient N.Request (N.Response L.ByteString -> a) deriving (Typeable, Functor)

httpClient :: (Member HttpClient r) => N.Request -> Eff r (N.Response L.ByteString)
httpClient req = send . inj $ HttpClient req id

runHttpClient :: (SetMember Lift (Lift IO) r) => N.Manager -> Eff (HttpClient :> r) a -> Eff r a
runHttpClient manager = loop
    where
    loop :: (SetMember Lift (Lift IO) r) => Eff (HttpClient :> r) a -> Eff r a
    loop = freeMap return $
        \u -> handleRelay u loop $
            \(HttpClient req f) -> lift (N.httpLbs req manager) >>= loop . f


runHttpClientMock :: (N.Request -> N.Response L.ByteString) -> Eff (HttpClient :> r) a -> Eff r a
runHttpClientMock server = loop
    where
        loop = freeMap return $
            \u -> handleRelay u loop $ \(HttpClient req f) -> loop . f $ server req
