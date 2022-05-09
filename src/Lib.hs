{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser, something, linkParser )
import Loader ( getHtml, loadData, getUrl )
import Graph ( createGraph, markLinks)
import InvertedIndex (invertedIndex)

searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages

  let mappedLinks = map (\s -> (getUrl s, linkHtmlParser $ getHtml s )) unpackedPages
  let mappedWords = map (\s -> (getUrl s, parser $ getHtml s )) unpackedPages
  let mappedInvertedWords = invertedIndex "sete" mappedWords

  --print mappedLinks
  --print mappedWords
  --print $ markLinks currentLinks otherLinks
  --print $ createGraph mappedLinks
  print mappedInvertedWords
  

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"

    searchEngine decoded

