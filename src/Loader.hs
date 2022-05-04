{-# LANGUAGE ScopedTypeVariables #-}

module Loader
    ( loadData,
    getUrl,
    getHtml
    ) where

import Data.Text ( Text,unpack)
import qualified Data.ByteString.Lazy
import Control.Applicative (Alternative (empty))
import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy (ByteString, readFile, split)
import Data.Maybe (isJust)
import Data.Aeson
  ( FromJSON (parseJSON),
    Value (Object),
    decode,
    (.:), eitherDecode,
  )

import PageData ( PageData(..) )

loadData :: FilePath -> IO [Maybe PageData]
loadData file = do
  res <- Data.ByteString.Lazy.readFile file
  let byLine = split (c2w '\n') res :: [ByteString]
  let decoded = map (\line -> decode line :: Maybe PageData) byLine
  return decoded

getUrl :: Maybe PageData -> String
getUrl (Just pageData) = unpack $ url pageData
getUrl Nothing = undefined

getHtml :: Maybe PageData -> String
getHtml (Just pageData) = unpack $ html_content pageData
getHtml Nothing = undefined

printUrl :: IO [Maybe PageData] -> IO ()
printUrl packedobject = do
  unpackedobject <- packedobject :: IO[Maybe PageData]
  mapM_ (print . getUrl) unpackedobject

printHtml :: IO [Maybe PageData] -> IO ()
printHtml packedobject = do
  unpackedobject <- packedobject :: IO[Maybe PageData]
  mapM_ (print . getHtml) unpackedobject


