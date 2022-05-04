module Main where

import Network.Wreq () -- requests
import Database.HDBC -- DB
import Lib (searchEngineModule)

main :: IO ()
main = searchEngineModule