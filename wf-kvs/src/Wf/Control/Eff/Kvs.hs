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
import Data.Typeable (Typeable)
import qualified Data.Binary as Bin (Binary)

type family KeyType kvs :: *

data Kvs kvs a =
    forall v. (Bin.Binary v) => Get kvs (KeyType kvs) (Maybe v -> a) |
    forall v. (Bin.Binary v) => Set kvs (KeyType kvs) v a |
    forall v. (Bin.Binary v) => SetWithTtl kvs (KeyType kvs) v Integer a |
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


get :: (Typeable kvs, Bin.Binary v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r (Maybe v)
get kvs k = send . inj . Get kvs k $ id

set :: (Typeable kvs, Bin.Binary v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Eff r ()
set kvs k v = send . inj . Set kvs k v $ ()

setWithTtl :: (Typeable kvs, Bin.Binary v, Member (Kvs kvs) r) => kvs -> KeyType kvs -> v -> Integer -> Eff r ()
setWithTtl kvs k v t = send . inj . SetWithTtl kvs k v t $ ()

delete :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r Bool
delete kvs k = send . inj . Delete kvs k $ id

exists :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r Bool
exists kvs k = send . inj . Exists kvs k $ id

ttl :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> KeyType kvs -> Eff r (Maybe Integer)
ttl kvs k = send . inj . Ttl kvs k $ id

keys :: (Typeable kvs, Member (Kvs kvs) r) => kvs -> Eff r [KeyType kvs]
keys kvs = send . inj . Keys kvs $ id

