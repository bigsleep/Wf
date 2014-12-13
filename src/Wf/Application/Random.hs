module Wf.Application.Random
( randomString
, randomByteString
) where

import System.Random.MWC (withSystemRandom, asGenIO, uniformR)
import Data.Array (Array, (!), array)
import qualified Data.ByteString.Char8 as B (ByteString, pack)
import Control.Monad (replicateM)

randomString :: Int -> IO String
randomString n = withSystemRandom . asGenIO $ r
    where
    r gen = map (chars' !) `fmap` replicateM n (uniformR range gen)
    chars = ['0' .. '9'] ++ ['a' .. 'z'] ++ ['A' .. 'Z']
    len = length chars
    range = (0, len - 1)
    chars' = array (0, len - 1) ([0..] `zip` chars) :: Array Int Char

randomByteString :: Int -> IO B.ByteString
randomByteString = fmap B.pack . randomString
