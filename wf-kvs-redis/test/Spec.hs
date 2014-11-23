module Main where

import Wf.Control.Eff.Run.Kvs.RedisSpec (kvsRedisSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    kvsRedisSpec
