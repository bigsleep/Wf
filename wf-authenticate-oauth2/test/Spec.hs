module Main where

import Wf.Web.Authenticate.OAuth2Spec (oauth2Spec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    oauth2Spec
