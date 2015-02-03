{-# LANGUAGE OverloadedStrings, DeriveDataTypeable, TypeFamilies #-}
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
, ErrorResponse(..)
, JsonRequest(..)
, JsonResponse(..)
, JsonParseError(..)
, defaultRequest
, defaultResponse
) where

import Control.Monad (mzero)
import Control.Applicative ((<$>))
import Control.Exception (Exception)
import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString, empty)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.Aeson as DA (ToJSON(..), FromJSON(..), Value(..), object, (.:), (.=))
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

newtype JsonRequest a = JsonRequest { unJsonRequest :: a } deriving (Show, Typeable, Eq)

instance (DA.ToJSON a) => DA.ToJSON (JsonRequest a) where
    toJSON (JsonRequest v) = DA.object ["input" DA..= DA.toJSON v]

instance (DA.FromJSON a) => DA.FromJSON (JsonRequest a) where
    parseJSON (DA.Object v) = JsonRequest <$> v DA..: "input"
    parseJSON _ = mzero

data JsonParseError = JsonParseError String deriving (Show, Typeable, Eq)

instance Control.Exception.Exception JsonParseError

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

newtype JsonResponse a = JsonResponse { unJsonResponse :: a } deriving (Show, Typeable, Eq)

newtype ErrorResponse = ErrorResponse { unErrorResponse :: Response L.ByteString } deriving (Show, Typeable, Eq)

instance (DA.ToJSON a) => DA.ToJSON (JsonResponse a) where
    toJSON (JsonResponse v) = DA.object ["output" DA..= DA.toJSON v]

instance (DA.FromJSON a) => DA.FromJSON (JsonResponse a) where
    parseJSON (DA.Object v) = JsonResponse <$> v DA..: "input"
    parseJSON _ = mzero

instance Exception ErrorResponse
