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
import Data.Tuple.Select ( Sel2(sel2), Sel1 (sel1) )


searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages

  let mappedLinks = map (\s -> (getUrl s, linkHtmlParser $ getHtml s )) unpackedPages
  let mappedLinksButDifferent = map(\s->map(\s2->(sel1 s,s2)) $ sel2 s) mappedLinks
  let mappedWords = map (\s -> (getUrl s, parser $ getHtml s )) unpackedPages
  let mappedInvertedWords = invertedIndex "sete" mappedWords

  print mappedLinksButDifferent
  --print mappedWords
  --print $ markLinks currentLinks otherLinks
  --print $ createGraph mappedLinks
  --print mappedInvertedWords

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded

