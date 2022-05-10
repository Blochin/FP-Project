{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser, something, linkParser, oneLinkParser )
import Loader ( getHtml, loadData, getUrl )
import Graph ( createGraph, markLinks, getAllIndexes)
import InvertedIndex (invertedIndex)
import Data.Tuple.Select ( Sel2(sel2), Sel1 (sel1) )


searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages

  let mappedLinks = map (\s -> (oneLinkParser $ getUrl s, linkHtmlParser $ getHtml s )) unpackedPages
  let mappedLinksButDifferent = map(\s->map(\s2->(sel1 s,s2)) $ sel2 s) mappedLinks
  let mappedWords = map (\s -> (getUrl s, parser $ getHtml s )) unpackedPages
  let mappedInvertedWords = invertedIndex "sete" mappedWords

  --print mappedLinksButDifferent
  --print mappedWords
  let markedLinks = markLinks currentLinks otherLinks
  print $ createGraph mappedLinksButDifferent markedLinks
  --print mappedInvertedWords
  print (getAllIndexes mappedLinksButDifferent markedLinks)

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded

