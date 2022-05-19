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
import Control.Monad


searchEngine :: IO [Maybe PageData] -> IO ()
searchEngine pages = do
  unpackedPages <- pages :: IO[Maybe PageData]
  
  let words = map(parser . getHtml) unpackedPages
  let currentLinks = linkParser $ map getUrl unpackedPages
  let otherLinks = map(linkHtmlParser . getHtml) unpackedPages

  let mappedLinks = map (\s -> (oneLinkParser $ getUrl s, linkHtmlParser $ getHtml s )) unpackedPages
  let mappedLinksButDifferent = map(\s-> filter(/= ("","")) $ map(\s2-> if(sel1 s /= s2) then (sel1 s,s2) else ("","")) $ sel2 s) mappedLinks
  let mappedWords = map (\s -> (getUrl s, parser $ getHtml s )) unpackedPages
  let invertedIndexMap = invertedIndex2 words mappedWords
  
  let markedLinks = markLinks currentLinks otherLinks
  let numIters = 10
  let dampingFactor = 0.75
  let inputForPageRank = getInputForPageRank mappedLinksButDifferent markedLinks
  -- v txt mame ulozeny graf - indexy stranok vo formate "'odkial' 'kam'" 
  -- inputFile <- readFile "data/input2.txt"
  -- na vstupe mame inputFile, pocet iteracii a damping factor nastaveny ako konstantu 0.85
  let pageRankWords = startPagerank inputForPageRank numIters dampingFactor  
--  tuto by bol ten nekonecny cyklus

--  writeFile "data/output2.txt" $ show $ startPagerank inputFile numIters dampingFactor

  let loop = do
            putStrLn "Search for word"
            word <- getLine
            let result = lookup word invertedIndexMap
            when (word /= "Stop Search") loop
            case result of
              Nothing  -> print "Zadane slovo sa nenaslo"
              Just result -> print $ indexWithPageRank result markedLinks pageRankWords
  loop

searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded
