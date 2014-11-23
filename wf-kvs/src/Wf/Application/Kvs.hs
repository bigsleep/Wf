{-# LANGUAGE DeriveDataTypeable, TypeFamilies, FlexibleContexts #-}
module Wf.Application.Kvs
( Kvs
, DefaultKvs(..)
, KvsError(..)
, get
, set
, setWithTtl
, delete
, exists
, ttl
, keys
) where

import Control.Eff (Eff, Member)
import qualified Wf.Control.Eff.Kvs (get, set, setWithTtl, delete, exists, ttl, keys, Kvs, KeyType)

import Control.Exception (Exception)

import Data.Typeable (Typeable)
import qualified Data.ByteString as B (ByteString)
import Wf.Data.Serializable (Serializable)

data DefaultKvs = DefaultKvs deriving (Show, Typeable)

type instance Wf.Control.Eff.Kvs.KeyType DefaultKvs = B.ByteString

type Kvs = Wf.Control.Eff.Kvs.Kvs DefaultKvs

data KvsError =
    KvsConnectionError String |
    KvsError String
    deriving (Show, Typeable)

instance Exception KvsError

get :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> Eff r (Maybe v)
get = Wf.Control.Eff.Kvs.get DefaultKvs

set :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Eff r ()
set = Wf.Control.Eff.Kvs.set DefaultKvs

setWithTtl :: (Typeable v, Serializable v, Member Kvs r) => B.ByteString -> v -> Integer -> Eff r ()
setWithTtl = Wf.Control.Eff.Kvs.setWithTtl DefaultKvs

delete :: (Member Kvs r) => B.ByteString -> Eff r Bool
delete = Wf.Control.Eff.Kvs.delete DefaultKvs

exists :: (Member Kvs r) => B.ByteString -> Eff r Bool
exists = Wf.Control.Eff.Kvs.exists DefaultKvs

ttl :: (Member Kvs r) => B.ByteString -> Eff r (Maybe Integer)
ttl = Wf.Control.Eff.Kvs.ttl DefaultKvs

keys :: (Member Kvs r) => Eff r [B.ByteString]
keys = Wf.Control.Eff.Kvs.keys DefaultKvs

