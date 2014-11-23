module Main where

import Wf.Control.Eff.Run.SessionSpec (sessionSpec)

import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
    sessionSpec
