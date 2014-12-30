{-# LANGUAGE OverloadedStrings #-}
module Settings
( Settings(..)
) where

import Control.Monad (mzero)
import qualified Data.Aeson as DA (Value(..), FromJSON(..), (.:))
import qualified Data.ByteString as B (ByteString)

import Wf.Web.Authenticate.OAuth2 (OAuth2Config)
import Wf.Session.Types (SessionSettings(..), SetCookieSettings(..))

data Settings = Settings
    { settingsPort :: Int
    , settingsUri :: B.ByteString
    , settingsOAuth2 :: OAuth2Config
    , settingsSession :: SessionSettings
    } deriving (Show)

instance DA.FromJSON Settings where
    parseJSON (DA.Object o) = do
        port <- o DA..: "port"
        uri <- o DA..: "uri"
        oauth2 <- o DA..: "oauth2"
        session <- o DA..: "session"
        return (Settings port uri oauth2 session)

    parseJSON _ = mzero


instance DA.FromJSON SetCookieSettings
instance DA.FromJSON SessionSettings
