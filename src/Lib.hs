{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser, something, linkParser )
import Loader ( getHtml, loadData, getUrl )
import Graph ( createGraph, markLinks)

searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages

  let mappedLinks = map (\s -> (getUrl s, concatMap(linkHtmlParser . getHtml) unpackedPages )) unpackedPages
  let mappedWords = map (\s -> (getUrl s, concatMap(parser . getHtml) unpackedPages )) unpackedPages

  print $ markLinks currentLinks otherLinks
  print $ createGraph mappedLinks
  

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded

