{-# LANGUAGE OverloadedStrings, FlexibleContexts, RankNTypes, DeriveDataTypeable #-}
module Wf.Web.JsonApi
( jsonApi
, jsonPostApi
, jsonGetApi
, JsonInput(..)
, JsonOutput(..)
, JsonParseError(..)
) where

import Control.Eff (Eff, Member)
import Control.Monad (mzero)
import Control.Applicative ((<$>))
import qualified Control.Exception (Exception)

import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, length)
import qualified Data.ByteString.Lazy.Char8 as L (unpack)
import qualified Data.Aeson as DA (FromJSON(..), ToJSON(..), Value(..), encode, decode, object, (.=), (.:))
import Data.Reflection (Given)
import Data.Typeable (Typeable)

import qualified Network.HTTP.Types as HTTP (status200, methodPost, methodGet, hContentType, hContentLength)
import qualified Network.Wai as Wai (Request, Response)

import Wf.Network.Http.Types (Request(..), Response(..))
import Wf.Web.Api (ApiDefinition(..), ApiInfo)
import Wf.Web.Routing (RouteDefinition(..), RouteMethod(..), parseRoute)
import Wf.Application.Exception (Exception, throwException)
import Wf.Application.Logger (Logger, logDebug)
import Wf.Network.Wai (FromWaiRequest(..), ToWaiResponse(..))

newtype JsonInput a = JsonInput { unJsonInput :: a } deriving (Show, Typeable, Eq)

instance (DA.ToJSON a) => DA.ToJSON (JsonInput a) where
    toJSON (JsonInput v) = DA.object ["input" DA..= DA.toJSON v]

instance (DA.FromJSON a) => DA.FromJSON (JsonInput a) where
    parseJSON (DA.Object v) = JsonInput <$> v DA..: "input"
    parseJSON _ = mzero

newtype JsonOutput a = JsonOutput { unJsonOutput :: a } deriving (Show, Typeable, Eq)

instance (DA.ToJSON a) => DA.ToJSON (JsonOutput a) where
    toJSON (JsonOutput v) = DA.object ["output" DA..= DA.toJSON v]

instance (DA.FromJSON a) => DA.FromJSON (JsonOutput a) where
    parseJSON (DA.Object v) = JsonOutput <$> v DA..: "input"
    parseJSON _ = mzero

data JsonParseError = JsonParseError String deriving (Show, Typeable, Eq)

instance Control.Exception.Exception JsonParseError

jsonApi
    :: (DA.FromJSON i, DA.ToJSON o, Member Exception r, Member Logger r)
    => (Wai.Request -> Eff r Wai.Response -> IO Wai.Response)
    -> String
    -> RouteDefinition
    -> (Given ApiInfo => i -> Eff r o)
    -> ApiDefinition
jsonApi run name route f =
    ApiDefinition
    { apiName = name
    , apiRouteDefinition = route
    , apiImplement = \request cont ->
        cont =<< run request . (return . toWaiResponse =<<) . app f =<< fromWaiRequest request
    }

    where
    app :: (DA.FromJSON i, DA.ToJSON o, Member Exception r, Member Logger r, Given ApiInfo)
        => (Given ApiInfo => i -> Eff r o) -> Request L.ByteString -> Eff r (Response L.ByteString)
    app g request = logDebug ("jsonApi request: " ++ show request) >> parse (requestBody request) >>= g >>= render

    parse :: (DA.FromJSON i, Member Exception r)
          => L.ByteString -> Eff r i
    parse input = maybe (onError input) (return . unJsonInput) . DA.decode $ input

    render :: (DA.ToJSON o, Member Exception r, Member Logger r)
           => o -> Eff r (Response L.ByteString)
    render output = do
        let body = DA.encode (JsonOutput output)
            contentType = (HTTP.hContentType, "application/json")
            contentLength = (HTTP.hContentLength, B.pack . show . L.length $ body)
            response = Response
                { responseStatus = HTTP.status200
                , responseHeaders = [contentType, contentLength]
                , responseBody = body
                }
        logDebug $ "jsonApi response: " ++ show response
        return response

    onError :: (Member Exception r) => L.ByteString -> Eff r a
    onError = throwException . JsonParseError . (++) "input: " . L.unpack

jsonPostApi, jsonGetApi
    :: (DA.FromJSON i, DA.ToJSON o, Member Exception r, Member Logger r)
    => (Wai.Request -> Eff r Wai.Response -> IO Wai.Response)
    -> String
    -> (Given ApiInfo => i -> Eff r o)
    -> ApiDefinition
jsonPostApi run route = jsonApi run route RouteDefinition { routeDefinitionMethod = RouteMethodSpecific HTTP.methodPost, routeDefinitionPattern = parseRoute route }
jsonGetApi run route = jsonApi run route RouteDefinition { routeDefinitionMethod = RouteMethodSpecific HTTP.methodGet, routeDefinitionPattern = parseRoute route }
