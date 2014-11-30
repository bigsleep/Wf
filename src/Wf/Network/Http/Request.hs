{-# LANGUAGE FlexibleContexts #-}
module Wf.Network.Http.Request
( Request(..)
, HttpVersion
, RequestMethod
, RequestHeader
, RequestQuery
, queryParam
) where

import Wf.Network.Http.Types (Request(..), HttpVersion, RequestMethod, RequestHeader, RequestQuery)

import Control.Monad (join)
import Control.Eff (Member, Eff)
import Control.Eff.Reader.Strict (Reader, ask)
import qualified Data.ByteString as B (ByteString)
import qualified Network.Wai as Wai (Request, queryString)

queryParam
    :: Member (Reader Wai.Request) r
    => B.ByteString -> Eff r (Maybe B.ByteString)
queryParam pname =
    return . join . lookup pname . Wai.queryString =<< ask
