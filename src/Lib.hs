{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser,linkParser, oneLinkParser )
import Loader ( getHtml, loadData, getUrl )
import Graph ( createGraph, markLinks, getAllIndexes, getInputForPageRank)
import Pagerank ( startPagerank)
import InvertedIndex (invertedIndex, invertedIndex2, indexWithPageRank)
import Data.Tuple.Select ( Sel2(sel2), Sel1 (sel1) )
import Data.List (nub)
import Data.Map ((!))
import Control.Monad ( when )
import MappingLinks ( mapLinks )


searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages
  let mappedLinks = mapLinks unpackedPages
  let mappedWords = map (\s -> (getUrl s, parser $ getHtml s )) unpackedPages
  let invertedIndexMap = invertedIndex2 words mappedWords
  
  let markedLinks = markLinks currentLinks otherLinks
  let numIters = 10
  let dampingFactor = 0.75
  let inputForPageRank = getInputForPageRank mappedLinks markedLinks
  let pageRankWords = startPagerank inputForPageRank numIters dampingFactor  
  --writeFile "data/output2.txt" $ show $ startPagerank inputForPageRank numIters dampingFactor

  let loop = do
            putStrLn "Search for word"
            word <- getLine
            let result = lookup word invertedIndexMap
            case result of
              Nothing  -> print "Search word not found"
              Just result -> print $ indexWithPageRank result markedLinks pageRankWords
            when (word /= "Stop Search") loop
  loop

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded
