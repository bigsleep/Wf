{-# LANGUAGE TypeOperators, FlexibleContexts, DeriveDataTypeable, ExistentialQuantification, TypeFamilies #-}
module Wf.Control.Eff.Kvs
( get
, set
, setWithTtl
, delete
, exists
, ttl
, keys
, Kvs(..)
, KeyType
) where

import Control.Eff (Eff, Member, inj, send)
import Wf.Data.Serializable (Serializable)
import Data.Typeable (Typeable)

type family KeyType kvs :: *

data Kvs kvs a =
    forall v. (Typeable v, Serializable v) => Get kvs (KeyType kvs) (Maybe v -> a) |
    forall v. (Typeable v, Serializable v) => Set kvs (KeyType kvs) v a |
    forall v. (Typeable v, Serializable v) => SetWithTtl kvs (KeyType kvs) v Integer a |
    Delete kvs (KeyType kvs) (Bool -> a) |
    Exists kvs (KeyType kvs) (Bool -> a) |
    Ttl kvs (KeyType kvs) (Maybe Integer -> a) |
    Keys kvs ([KeyType kvs] -> a)
    deriving (Typeable)

instance Functor (Kvs kvs) where
    fmap f (Get kvs k c) = Get kvs k (f . c)
    fmap f (Set kvs k v c) = Set kvs k v (f c)
    fmap f (SetWithTtl kvs k v t c) = SetWithTtl kvs k v t (f c)
    fmap f (Delete kvs k c) = Delete kvs k (f . c)
    fmap f (Exists kvs k c) = Exists kvs k (f . c)
    fmap f (Ttl kvs k c) = Ttl kvs k (f . c)
    fmap f (Keys kvs c) = Keys kvs (f . c)


get :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r (Maybe v)
get kvs k = send $ inj . Get kvs k

set :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Eff r ()
set kvs k v = send $ \f -> inj . Set kvs k v $ f ()

setWithTtl :: (Typeable kvs, Typeable v, Serializable v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Integer -> Eff r ()
setWithTtl kvs k v t = send $ \f -> inj . SetWithTtl kvs k v t $ f ()

delete :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r Bool
delete kvs k = send $ inj . Delete kvs k

exists :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r Bool
exists kvs k = send $ inj . Exists kvs k

ttl :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r (Maybe Integer)
ttl kvs k = send $ inj . Ttl kvs k

keys :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> Eff r [KeyType kvs]
keys kvs = send $ inj . Keys kvs

