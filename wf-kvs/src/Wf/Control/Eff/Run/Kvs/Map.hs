{-# LANGUAGE TypeOperators, FlexibleContexts, TypeFamilies #-}
module Wf.Control.Eff.Run.Kvs.Map
( runKvsMap
) where

import Control.Eff (Eff, (:>), Member, freeMap, handleRelay)
import Control.Eff.State.Strict (State, get, modify)
import qualified Wf.Control.Eff.Kvs as Kvs (Kvs(..), KeyType)

import qualified Data.Map as M (Map, lookup, insert, delete, member, keys)
import qualified Data.ByteString as B (ByteString)
import qualified Data.ByteString.Lazy as L (ByteString)
import Data.Typeable (Typeable)
import qualified Data.Binary as Bin (encode, decodeOrFail)

runKvsMap :: (Typeable kvs, Member (State (M.Map B.ByteString L.ByteString)) r, Kvs.KeyType kvs ~ B.ByteString) => Eff (Kvs.Kvs kvs :> r) a -> Eff r a
runKvsMap = loop
    where
    loop = freeMap return $ \u -> handleRelay u loop handle

    handle (Kvs.Get _ k c) = do
        m <- get
        let r = either (const Nothing) (\(_,_,a) -> Just a) . Bin.decodeOrFail =<< M.lookup k m
        loop (c r)

    handle (Kvs.Set _ k v c) = modify (M.insert k (Bin.encode v)) >> loop c

    handle (Kvs.Delete _ k c) = modify f >> loop (c True)
        where
        f :: M.Map B.ByteString L.ByteString -> M.Map B.ByteString L.ByteString
        f = M.delete k

    handle (Kvs.SetWithTtl kvs k v _ c)  = handle (Kvs.Set kvs k v c)

    handle (Kvs.Exists _ k c) = do
        m <- get
        loop . c $ M.member k (m :: M.Map B.ByteString L.ByteString)

    handle (Kvs.Ttl _ _ c) = loop . c $ Nothing

    handle (Kvs.Keys _ c) = do
        m <- get
        loop . c $ M.keys (m :: M.Map B.ByteString L.ByteString)
