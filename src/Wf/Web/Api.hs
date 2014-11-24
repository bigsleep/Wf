{-# LANGUAGE TypeOperators, DeriveDataTypeable, FlexibleContexts, RankNTypes, ExistentialQuantification #-}
module Wf.Web.Api
( apiRoutes
, ApiDefinition(..)
, ApiInfo(..)
, getParameter
, createApi
, getApi
, postApi
, createApiWith
, getApiWith
, postApiWith
) where

import Control.Eff (SetMember, Eff, (:>))
import Control.Eff.Lift (Lift, lift)

import qualified Data.List as L (lookup)
import qualified Data.ByteString as B (ByteString)
import Data.Typeable (Typeable)
import Data.Reflection (Given, give, given)
import qualified Wf.Web.Routing as R (RouteDefinition(..), RouteMethod(..), Parameter, route, routes, parseRoute)
import qualified Network.HTTP.Types as HTTP (Method, methodGet, methodPost)
import qualified Wf.Network.Wai as Wai (App(..), FromWaiRequest(..), ToWaiResponse(..))
import qualified Network.Wai as Wai (Request, Response, requestMethod, rawPathInfo)

data ApiDefinition m = ApiDefinition
    { apiName :: String
    , apiRouteDefinition :: R.RouteDefinition
    , apiImplement :: (Given ApiInfo) => Wai.App m
    , apiBefore :: m ()
    , apiAfter :: m ()
    } deriving (Typeable)

data ApiInfo = ApiInfo
    { apiInfoApiName :: String
    , apiInfoRouteDefinition :: R.RouteDefinition
    , apiInfoParameters :: [R.Parameter]
    } deriving (Typeable)

apiRoutes
    :: (Wai.ToWaiResponse response, SetMember Lift (Lift IO) r)
    => Eff r response
    -> [ApiDefinition (Eff r)]
    -> Wai.Request
    -> Eff r Wai.Response
apiRoutes defaultApp apis request = R.routes def (map entry apis) method path
    where
    method = Wai.requestMethod request
    path = Wai.rawPathInfo request
    def = fmap Wai.toWaiResponse defaultApp
    entry api = R.route (apiRouteDefinition api) (exec api)
    wrap (Wai.App a) req = do
        x <- lift (Wai.fromWaiRequest req)
        y <- a x
        return (Wai.toWaiResponse y)
    exec api parameters = do
        apiBefore api
        let apiInfo = ApiInfo { apiInfoApiName = apiName api
                              , apiInfoRouteDefinition = apiRouteDefinition api
                              , apiInfoParameters = parameters
                              }
        r <- wrap (give apiInfo (apiImplement api)) $ request
        apiAfter api
        return r

getParameter :: Given ApiInfo => B.ByteString -> Maybe B.ByteString
getParameter name = L.lookup name $ apiInfoParameters given

createApi :: (Monad m) => String -> R.RouteDefinition -> (Given ApiInfo => Wai.App m) -> ApiDefinition m
createApi name route app =
    ApiDefinition
    { apiName = name
    , apiRouteDefinition = route
    , apiImplement = app
    , apiBefore = return ()
    , apiAfter = return ()
    }

getApi, postApi :: (Monad m, Wai.FromWaiRequest request, Wai.ToWaiResponse response)
    => String -> (Given ApiInfo => request -> m response) -> ApiDefinition m
getApi route app = createApi route rd (Wai.App app)
    where
    rd = R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodGet, R.routeDefinitionPattern = R.parseRoute route }
postApi route app = createApi route rd (Wai.App app)
    where
    rd = R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodPost, R.routeDefinitionPattern = R.parseRoute route }

createApiWith
    :: (Monad m, Wai.FromWaiRequest request, Wai.ToWaiResponse response)
    => String
    -> R.RouteDefinition
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition m
createApiWith name route parser implement renderer =
    createApi name route $ Wai.App (\request -> parser request >>= implement >>= renderer)

getApiWith, postApiWith
    :: (Monad m, Wai.FromWaiRequest request, Wai.ToWaiResponse response)
    => String
    -> (Given ApiInfo => request -> m input)
    -> (Given ApiInfo => input -> m output)
    -> (Given ApiInfo => output -> m response)
    -> ApiDefinition m
getApiWith route = createApiWith route rd
    where
    rd = R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodGet, R.routeDefinitionPattern = R.parseRoute route }
postApiWith route = createApiWith route rd
    where
    rd = R.RouteDefinition { R.routeDefinitionMethod = R.RouteMethodSpecific HTTP.methodPost, R.routeDefinitionPattern = R.parseRoute route }

