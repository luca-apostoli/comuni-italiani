module Main where

import System.Environment (lookupEnv)
import Lib

main :: IO ()
main = lookupEnv "PORT" >>= startApp
