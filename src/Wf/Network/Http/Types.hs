{-# LANGUAGE DeriveDataTypeable, TypeFamilies #-}
module Wf.Network.Http.Types
( Request(..)
, HttpVersion
, RequestMethod
, RequestHeader
, RequestQuery
, UrlEncoded(..)
, Response(..)
, ResponseStatus
, ResponseHeader
, ResponseFilePath(..)
, defaultRequest
, defaultResponse
) where

import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Network.HTTP.Types as HTTP (HttpVersion, Header, Method, Status, Query, http10, methodGet, status200)
import Network.Socket (SockAddr(..))

type HttpVersion = HTTP.HttpVersion

type RequestMethod = HTTP.Method

type RequestHeader = HTTP.Header

type RequestQuery = HTTP.Query

type RequestRemoteHost = SockAddr

data Request body = Request
    { requestHttpVersion :: HttpVersion
    , requestMethod :: RequestMethod
    , requestHeaders :: [RequestHeader]
    , requestPath :: [B.ByteString]
    , requestRawPath :: B.ByteString
    , requestQuery :: RequestQuery
    , requestRawQuery :: B.ByteString
    , requestRemoteHost :: RequestRemoteHost
    , requestBody :: body
    , requestIsSecure :: Bool
    } deriving (Show, Typeable, Eq)

newtype UrlEncoded = UrlEncoded { unUrlEncoded :: HTTP.Query }

type ResponseStatus = HTTP.Status

type ResponseHeader = HTTP.Header

newtype ResponseFilePath = ResponseFilePath { unResponseFilePath :: FilePath }

data Response body = Response
    { responseStatus :: ResponseStatus
    , responseHeaders :: [ResponseHeader]
    , responseBody :: body
    } deriving (Show, Typeable, Eq)

defaultRequest :: Request ()
defaultRequest = Request
    { requestHttpVersion = HTTP.http10
    , requestMethod = HTTP.methodGet
    , requestHeaders = []
    , requestPath = []
    , requestRawPath = B.empty
    , requestQuery = []
    , requestRawQuery = B.empty
    , requestIsSecure = False
    , requestRemoteHost = SockAddrInet (toEnum 0) (toEnum 0)
    , requestBody = ()
    }

defaultResponse :: a -> Response a
defaultResponse a = Response
    { responseStatus = HTTP.status200
    , responseHeaders = []
    , responseBody = a
    }

instance (Typeable body, Show body) => Exception (Response body)
