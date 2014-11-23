module Main where

import Wf.Web.RoutingSpec (routingSpec)
import Wf.Web.JsonApiSpec (jsonApiSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    routingSpec
    jsonApiSpec
