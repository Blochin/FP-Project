{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( loader
    ) where

import Data.Text ( Text,unpack)
import qualified Data.Text as T
import Data.ByteString ()
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
  
import Parser(something, parser)
import PageData ( PageData(..) )

loadParsedData :: FilePath -> IO [Maybe PageData]
loadParsedData file = do
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
  mapM_ (print . getHtml) unpackedobject

getAllHtmlContent :: IO [Maybe PageData] -> IO ()
getAllHtmlContent pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  let array = map(\s -> parser $ getHtml s) unpackedPages
  --let array = map(parser . showHtml) unpackedPages it's same as let array = map(\s -> parser $ showHtml s) unpackedPages
  print array

loader :: IO ()
loader = do
    let decoded = loadParsedData "data/data.json"
    getAllHtmlContent decoded