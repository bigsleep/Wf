{-# LANGUAGE TypeOperators, OverloadedStrings, FlexibleContexts, TemplateHaskell #-}
module Wf.Web.JsonApiSpec
( jsonApiSpec
) where

import Control.Eff (Eff, Member, (:>))
import Control.Eff.Exception (Exc, runExc)
import Control.Eff.Lift (Lift, runLift)
import Control.Monad (sequence_)
import Control.Exception (SomeException(..))

import qualified Network.HTTP.Types as HTTP (Method, status200, status400, status404, methodGet, methodPost, hContentType, hContentLength)
import qualified Network.Wai as Wai (Application, Request, Response, requestMethod)
import qualified Network.Wai.Test as WT

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString, length)
import Data.Typeable (cast)
import Data.Either (isLeft)
import Data.Maybe (isJust)
import qualified Data.Aeson as DA (FromJSON(..), ToJSON(..), Value(..), encode, decode, object, (.=), (.:))
import qualified Data.Aeson.TH as DA (deriveJSON, defaultOptions)
import qualified Data.Map as M

import Wf.Web.Api (apiRoutes)
import Wf.Web.JsonApi (jsonApi, jsonPostApi, jsonGetApi, JsonInput(..), JsonOutput(..), JsonParseError)
import Wf.Network.Http.Types (Request(..), Response(..), defaultRequest, defaultResponse)
import Wf.Network.Http.Response (setStatus, setBody)
import Wf.Network.Wai (toWaiResponse)
import Wf.Application.Exception (Exception)
import Wf.Application.Logger (Logger)
import Wf.Control.Eff.Logger (runLoggerStdIO, LogLevel(..))

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, expectationFailure)

jsonApiSpec :: Spec
jsonApiSpec = describe "json api" . it "create json api" $ do
    let rootInput = ()
    execCase HTTP.methodGet "/" rootInput (shouldResponseNormal . rootApp $ rootInput)

    let rootBadInput = "admin" :: String
    execCase HTTP.methodGet "/" rootBadInput (shouldError 400)

    let addInput = (1, 2) :: (Int, Int)
    execCase HTTP.methodGet "/add" addInput (shouldResponseNormal (3 :: Int))

    let addBadInput = (1, 2, 3) :: (Int, Int, Int)
    execCase HTTP.methodGet "/add" addBadInput (shouldError 400)

    let dicInput = [1, 2, 3, 4, 5] :: [Int]
    execCase HTTP.methodPost "/dic" dicInput (shouldResponseNormal . dicApp $ dicInput)

    let dicBadInput = ["1", "2", "3", "4"] :: [String]
    execCase HTTP.methodPost "/dic" dicBadInput (shouldError 400)

    -- not found case
    execCase HTTP.methodGet "/xxx" () (shouldError 404)

    where
    execCase :: (DA.ToJSON a) => HTTP.Method -> B.ByteString -> a -> (WT.SRequest -> WT.Session ()) -> IO ()
    execCase method path a s =
        WT.runSession (s sreq) testApp
        where
        body = DA.encode (JsonInput a)
        req = WT.setRawPathInfo WT.defaultRequest {Wai.requestMethod = method} path
        sreq = WT.SRequest { WT.simpleRequest = req, WT.simpleRequestBody = body }

    shouldResponseNormal :: (DA.ToJSON a) => a -> WT.SRequest -> WT.Session ()
    shouldResponseNormal body sreq  = do
        r <- WT.srequest sreq
        let body' = DA.encode . JsonOutput $ body
        WT.assertStatus 200 r
        WT.assertContentType "application/json" r
        WT.assertHeader HTTP.hContentLength (B.pack . show . L.length $ body') r
        WT.assertBody body' r

    shouldError code sreq = do
        r <- WT.srequest sreq
        WT.assertStatus code r

type M = Eff (Exception :> Logger :> Lift IO :> ())

testApp :: Wai.Application
testApp = apiRoutes notFoundApp
    [ jsonGetApi run "/" (return . rootApp)
    , jsonGetApi run "/add" (return . addApp)
    , jsonPostApi run "/dic" (return . dicApp)
    ]
    where
    run :: M Wai.Response -> IO Wai.Response
    run = (handleResult =<<) . runLift . runLoggerStdIO DEBUG . runExc
    handleResult (Right r) = return r
    handleResult (Left _) = return . toWaiResponse . setStatus HTTP.status400 . defaultResponse $ ()

notFoundApp :: Response L.ByteString
notFoundApp = setBody "<h1>Not Found</h1>" . setStatus HTTP.status404 $ defaultResponse ()

rootApp :: () -> String
rootApp _ = "<p>hello</p>"

addApp :: (Integer, Integer) -> Integer
addApp (a, b) = a + b

dicApp :: [Int] -> M.Map String Int
dicApp xs = M.fromList $ zip (map show xs) xs
