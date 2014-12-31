{-# LANGUAGE TypeOperators, FlexibleContexts, FlexibleInstances, ExistentialQuantification #-}
module Wf.Network.Wai
( FromWaiRequest(..)
, ToWaiResponse(..)
, toWaiApplication
, UrlEncoded(..)
) where

import qualified Data.ByteString.Lazy as L (ByteString, empty)
import qualified Data.Text.Encoding as T (encodeUtf8)
import qualified Network.Wai as Wai (Application, Request, httpVersion, requestMethod, requestHeaders, requestBody, pathInfo, rawPathInfo, queryString, rawQueryString, isSecure, remoteHost, strictRequestBody, Response, responseLBS, responseFile)
import qualified Network.HTTP.Types as HTTP (parseQuery)
import Wf.Network.Http.Types (Request(..), Response(..), ResponseFilePath(..), UrlEncoded(..))

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
    fromWaiRequest w =
        return . fromWaiRequest' w =<< Wai.strictRequestBody w

instance FromWaiRequest (Request ()) where
    fromWaiRequest = return . flip fromWaiRequest' ()

instance FromWaiRequest (Request UrlEncoded) where
    fromWaiRequest w = return . fromWaiRequest' w . UrlEncoded . HTTP.parseQuery =<< Wai.requestBody w

fromWaiRequest' :: Wai.Request -> body -> Request body
fromWaiRequest' w b = Request
    { requestHttpVersion = Wai.httpVersion w
    , requestMethod = Wai.requestMethod w
    , requestHeaders = Wai.requestHeaders w
    , requestPath = fmap T.encodeUtf8 . Wai.pathInfo $ w
    , requestRawPath =  Wai.rawPathInfo w
    , requestQuery = Wai.queryString w
    , requestRawQuery = Wai.rawQueryString w
    , requestRemoteHost = Wai.remoteHost w
    , requestIsSecure = Wai.isSecure w
    , requestBody = b
    }

instance ToWaiResponse Wai.Response where
    toWaiResponse = id

instance ToWaiResponse (Response L.ByteString) where
    toWaiResponse res = Wai.responseLBS (responseStatus res) (responseHeaders res) (responseBody res)

instance ToWaiResponse (Response ()) where
    toWaiResponse res = Wai.responseLBS (responseStatus res) (responseHeaders res) L.empty

instance ToWaiResponse (Response ResponseFilePath) where
    toWaiResponse res = Wai.responseFile (responseStatus res) (responseHeaders res) (unResponseFilePath $ responseBody res) Nothing
