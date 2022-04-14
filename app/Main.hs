
module Main where

import Lib
import Network.Wreq -- requests
import Database.HDBC -- DB


main :: IO ()
main = do
    rsp <- get "https://www.youtube.com/"
    print rsp
  