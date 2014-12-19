{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ExistentialQuantification #-}
module Wf.Network.Wai
( FromWaiRequest(..)
, ToWaiResponse(..)
, toWaiApplication
) where

import qualified Data.ByteString.Lazy as L (ByteString, empty)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Network.Wai as Wai (Application, Request, httpVersion, requestMethod, requestHeaders, pathInfo, rawPathInfo, queryString, rawQueryString, isSecure, remoteHost, strictRequestBody, Response, responseLBS, responseFile)
import Wf.Network.Http.Types (Request(..), Response(..))

toWaiApplication
    :: (FromWaiRequest request, ToWaiResponse response)
    => (request -> IO response) -> Wai.Application
toWaiApplication app w respond = fromWaiRequest w >>= app >>= respond . toWaiResponse

class FromWaiRequest a where
    fromWaiRequest :: Wai.Request -> IO a

class ToWaiResponse a where
    toWaiResponse :: a -> Wai.Response

instance FromWaiRequest (Wai.Request) where
    fromWaiRequest = return

instance FromWaiRequest (Request L.ByteString) where
    fromWaiRequest w = do
        body <- Wai.strictRequestBody w
        return Request
            { requestHttpVersion = Wai.httpVersion w
            , requestMethod = Wai.requestMethod w
            , requestHeaders = Wai.requestHeaders w
            , requestPath = fmap T.encodeUtf8 . Wai.pathInfo $ w
            , requestRawPath =  Wai.rawPathInfo w
            , requestQuery = Wai.queryString w
            , requestRawQuery = Wai.rawQueryString w
            , requestRemoteHost = Wai.remoteHost w
            , requestIsSecure = Wai.isSecure w
            , requestBody = body
            }

instance FromWaiRequest (Request ()) where
    fromWaiRequest w =
        return Request
            { requestHttpVersion = Wai.httpVersion w
            , requestMethod = Wai.requestMethod w
            , requestHeaders = Wai.requestHeaders w
            , requestPath = fmap T.encodeUtf8 . Wai.pathInfo $ w
            , requestRawPath =  Wai.rawPathInfo w
            , requestQuery = Wai.queryString w
            , requestRawQuery = Wai.rawQueryString w
            , requestRemoteHost = Wai.remoteHost w
            , requestIsSecure = Wai.isSecure w
            , requestBody = ()
            }

instance ToWaiResponse Wai.Response where
    toWaiResponse = id

instance ToWaiResponse (Response L.ByteString) where
    toWaiResponse res = Wai.responseLBS (responseStatus res) (responseHeaders res) (responseBody res)

instance ToWaiResponse (Response ()) where
    toWaiResponse res = Wai.responseLBS (responseStatus res) (responseHeaders res) L.empty

instance ToWaiResponse (Response FilePath) where
    toWaiResponse res = Wai.responseFile (responseStatus res) (responseHeaders res) (responseBody res) Nothing
