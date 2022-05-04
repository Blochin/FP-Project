{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser )
import Loader ( getHtml, loadData )

searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  let array = map(parser . getHtml) unpackedPages
  --let array = map(parser . showHtml) unpackedPages it's same as let array = map(\s -> parser $ showHtml s) unpackedPages
  print array

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded