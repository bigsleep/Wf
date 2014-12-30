{-# LANGUAGE OverloadedStrings #-}
module Wf.Network.Http.Response
( Response
, setStatus
, addHeader
, setHeaders
, setBody
, redirect
, defaultResponse
, setContentType
, setContentLength
, setCookie
, setCookie'
, text
, html
, json
, file
) where

import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.ByteString.Lazy as L (ByteString, length)
import qualified Blaze.ByteString.Builder as Blaze (toByteString)
import qualified Network.HTTP.Types as HTTP (status302, hContentType, hContentLength)
import Wf.Application.Time (Time)
import Wf.Network.Http.Types (Response(..), ResponseStatus, ResponseHeader, ResponseFilePath(..), defaultResponse)
import qualified Web.Cookie as Cookie (SetCookie, renderSetCookie, def, setCookieName, setCookieValue, setCookieExpires, setCookieSecure, setCookieHttpOnly, setCookieDomain, setCookiePath)

setStatus :: ResponseStatus -> Response body -> Response body
setStatus s res = res { responseStatus = s }

addHeader :: ResponseHeader -> Response body -> Response body
addHeader h res = res { responseHeaders = h : responseHeaders res }

setHeaders :: [ResponseHeader] -> Response body -> Response body
setHeaders hs res = res { responseHeaders = hs }

setBody :: body -> Response a -> Response body
setBody body res = res { responseBody = body }

redirect :: B.ByteString -> Response body -> Response body
redirect url res =
    let status = HTTP.status302
        header = ("Location", url)
    in res { responseStatus = status, responseHeaders = header : responseHeaders res }

setContentType :: B.ByteString -> Response a -> Response a
setContentType ctype = addHeader (HTTP.hContentType, ctype)

setContentLength :: Integer -> Response a -> Response a
setContentLength l = addHeader (HTTP.hContentLength, B.pack . show $ l)

setCookie :: B.ByteString -> B.ByteString -> Maybe Time -> Maybe B.ByteString -> Maybe B.ByteString -> Bool -> Bool -> Response a -> Response a
setCookie name value expires domain path secure httpOnly =
    addHeader ("Set-Cookie", Blaze.toByteString . Cookie.renderSetCookie $ sc)
    where
    sc = Cookie.def
        { Cookie.setCookieName = name
        , Cookie.setCookieValue = value
        , Cookie.setCookieExpires = expires
        , Cookie.setCookieSecure = secure
        , Cookie.setCookieHttpOnly = httpOnly
        , Cookie.setCookieDomain = domain
        , Cookie.setCookiePath = path
        }

setCookie' :: Cookie.SetCookie -> Response a -> Response a
setCookie' sc = addHeader ("Set-Cookie", Blaze.toByteString . Cookie.renderSetCookie $ sc)

text :: L.ByteString -> Response a -> Response L.ByteString
text b = setContentType "text/plain" . setContentLength (fromIntegral . L.length $ b) . setBody b

html :: L.ByteString -> Response a -> Response L.ByteString
html b = setContentType "text/html" . setContentLength (fromIntegral . L.length $ b) . setBody b

json :: L.ByteString -> Response a -> Response L.ByteString
json b = setContentType "application/json" . setContentLength (fromIntegral . L.length $ b) . setBody b

file :: FilePath -> Response a -> Response ResponseFilePath
file = setBody . ResponseFilePath
