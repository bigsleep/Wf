{-# LANGUAGE OverloadedStrings, TypeOperators, FlexibleContexts, TypeFamilies, QuasiQuotes #-}
module Wf.Control.Eff.Run.Kvs.Redis
( runKvsRedis
) where

import Control.Eff (Eff, VE(..), (:>), Member, SetMember, admin, handleRelay)
import Control.Eff.Lift (Lift, lift)
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs(..), KeyType)

import qualified Database.Redis as Redis (get, set, setex, del, exists, ttl, keys, ConnectInfo, connect, runRedis, Status(..))
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (toStrict, fromStrict)
import Data.Typeable (Typeable)
import Wf.Data.Serializable (serialize, deserialize)
import Text.Printf.TH (s)

import Wf.Application.Kvs (KvsError(..))
import Wf.Application.Logger (Logger, logDebug, logError)
import Wf.Application.Exception (Exception, throwException, liftException)

runKvsRedis :: ( Typeable kvs,
                 SetMember Lift (Lift IO) r,
                 Member Logger r,
                 Member Exception r,
                 Kvs.KeyType kvs ~ B.ByteString
               )
            => Redis.ConnectInfo -> Eff (Kvs.Kvs kvs :> r) a -> Eff r a
runKvsRedis cinfo eff = do
    cn <- liftException $ Redis.connect cinfo
    loop cn . admin $ eff

    where loop _ (Val a) = return a

          loop cn (E u) = handleRelay u (loop cn) (handle cn)

          handleRedis cn redis handleResult' = (lift . Redis.runRedis cn $ redis) >>= handleResult'

          handle cn (Kvs.Get _ k c) = handleRedis cn (Redis.get k) handleResult
            where handleResult (Right x) = loop cn . c $ (x >>= deserialize . L.fromStrict)

                  handleResult (Left x) = do
                    logError $ [s|redis get failure. key=%s reply=%s|] k (show x)
                    throwException $ KvsError "redis get failure."

          handle cn (Kvs.Set _ k v c) = handleRedis cn (Redis.set k . L.toStrict . serialize $ v) handleResult
            where handleResult (Right Redis.Ok) = do
                    logDebug $ [s|redis set success. key=%s value=%s status=Ok|] k (serialize v)
                    loop cn c

                  handleResult r = do
                    logError $ [s|redis set failure. key=%s value=%s reply=%s|] k (serialize v) (show r)
                    throwException $ KvsError "kvs set failure."

          handle cn (Kvs.SetWithTtl _ k v ttl c) = handleRedis cn (Redis.setex k ttl . L.toStrict . serialize $ v) handleResult
            where handleResult (Right Redis.Ok) = do
                    logDebug $ [s|redis setex success. key=%s ttl=%d value=%s status=Ok|] k ttl (serialize v)
                    loop cn c

                  handleResult x = do
                    logError $ [s|redis setex failure. key=%s ttl=%d value=%s reply=%s|] k ttl (serialize v) (show x)
                    throwException $ KvsError "set with ttl failure."

          handle cn (Kvs.Delete _ k c) = handleRedis cn (Redis.del [k]) handleResult
            where handleResult (Right x) = do
                    logDebug $ [s|redis del success. key=%s deleted=%d|] k x
                    loop cn . c $ (x == 1)

                  handleResult (Left x) = do
                    logError $ [s|redis del failure. key=%s reply=%s|] k (show x)
                    throwException $ KvsError "delete failure."

          handle cn (Kvs.Exists _ k c) = handleRedis cn (Redis.exists k) handleResult
            where handleResult (Right x) = do
                    logDebug $ [s|redis exists success. key=%s result=%s|] k (show x)
                    loop cn . c $ x

                  handleResult (Left x) = do
                    logError $ [s|redis exists failure. key=%s reply=%s|] k (show x)
                    throwException $ KvsError "exists failure."

          handle cn (Kvs.Ttl _ k c) = handleRedis cn (Redis.ttl k) handleResult
            where handleResult (Right x) = do
                    logDebug $ [s|redis ttl success. key=%s result=%d|] k x
                    loop cn . c $ if x >= 0 then Just x else Nothing

                  handleResult (Left x) = do
                    logError $ [s|redis ttl failure. key=%s reply=%s|] k (show x)
                    throwException $ KvsError "ttl failure."

          handle cn (Kvs.Keys _ c) = handleRedis cn (Redis.keys "*") handleResult
            where handleResult (Right x) = loop cn . c $ x

                  handleResult (Left x) = do
                    logError $ [s|redis get failure. reply=%s|] (show x)
                    throwException $ KvsError "redis get failure."

