{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser, something, linkParser )
import Loader ( getHtml, loadData, getUrl )

searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]

  let currentLinks = linkParser $ map(\s -> getUrl s) unpackedPages
  let words = map(parser . getHtml) unpackedPages
  let links = map(linkHtmlParser . getHtml) unpackedPages
  
  print links

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded