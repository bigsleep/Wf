{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiInfo(..)
, urlParam
, getWai
, postWai
, createApi
, getApi
, postApi
, createApiWith
, getApiWith
, postApiWith
) where

import qualified Data.List as L (lookup)
import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import Data.Reflection (Given, give, given)
import qualified Wf.Web.Routing as R (RouteDefinition(..), RouteMethod(..), Parameter, route, routes, parseRoute)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)
import Wf.Network.Wai (FromWaiRequest(..), ToWaiResponse(..))
import qualified Network.Wai as Wai (Application, Request, Response, requestMethod, rawPathInfo)

data ApiDefinition = ApiDefinition
    { apiName :: String
    , apiRouteDefinition :: R.RouteDefinition
    , apiImplement :: (Given ApiInfo) => Wai.Application
    } deriving (Typeable)

data ApiInfo = ApiInfo
    { apiInfoApiName :: String
    , apiInfoRouteDefinition :: R.RouteDefinition
    , apiInfoParameters :: [R.Parameter]
    } deriving (Typeable)

apiRoutes
    :: Wai.Application
    -> [ApiDefinition]
    -> Wai.Application
apiRoutes defaultApp apis request = R.routes defaultApp (map entry apis) method path request
    where
    method = Wai.requestMethod request
    path = Wai.rawPathInfo request
    entry api = R.route (apiRouteDefinition api) (exec api)
    exec api parameters = do
        let apiInfo = ApiInfo { apiInfoApiName = apiName api
                              , apiInfoRouteDefinition = apiRouteDefinition api
                              , apiInfoParameters = parameters
                              }
        give apiInfo (apiImplement api)

urlParam :: Given ApiInfo => B.ByteString -> Maybe B.ByteString
urlParam name = L.lookup name $ apiInfoParameters given

getWai, postWai
    :: String
    -> (Given ApiInfo => Wai.Application)
    -> ApiDefinition
getWai route = ApiDefinition route (rdget route)
postWai route =  ApiDefinition route (rdpost route)

createApi
    :: (Monad m, FromWaiRequest request, ToWaiResponse response)
    => (Wai.Request -> m Wai.Response -> IO Wai.Response)
    -> String
    -> R.RouteDefinition
    ->(Given ApiInfo => request -> m response)
    -> ApiDefinition
createApi run name route app =
    ApiDefinition
    { apiName = name
    , apiRouteDefinition = route
    , apiImplement = \request cont ->
        cont =<< run request . (return . toWaiResponse =<<) . app =<< fromWaiRequest request
    }

getApi, postApi
    :: (Monad m, FromWaiRequest request, ToWaiResponse response)
    => (Wai.Request -> m Wai.Response -> IO Wai.Response)
    -> String
    -> (Given ApiInfo => request -> m response)
    -> ApiDefinition
getApi run route = createApi run route (rdget route)
postApi run route = createApi run route (rdpost route)

createApiWith
    :: (Monad m, FromWaiRequest request, ToWaiResponse response)
    => (Wai.Request -> m Wai.Response -> IO Wai.Response)
    -> String
    -> R.RouteDefinition
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition
createApiWith run name route parser implement renderer =
    createApi run name route $
        \request -> parser request >>= implement >>= renderer

getApiWith, postApiWith
    :: (Monad m, FromWaiRequest request, ToWaiResponse response)
    => (Wai.Request -> m Wai.Response -> IO Wai.Response)
    -> String
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition
getApiWith run route = createApiWith run route (rdget route)
postApiWith run route = createApiWith run route (rdpost route)

rd :: HTTP.Method -> String -> R.RouteDefinition
rd method route = R.RouteDefinition (R.RouteMethodSpecific method) (R.parseRoute route)

rdget, rdpost :: String -> R.RouteDefinition
rdget = rd HTTP.methodGet
rdpost = rd HTTP.methodPost
