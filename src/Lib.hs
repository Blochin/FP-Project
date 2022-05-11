{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( searchEngineModule
    ) where

import PageData ( PageData )
import Parser ( parser, linkHtmlParser, something, linkParser, oneLinkParser )
import Loader ( getHtml, loadData, getUrl )
import Graph ( createGraph, markLinks, getAllIndexes, getInputForPageRank)
import Pagerank ( startPagerank)
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
  let markedLinks = markLinks currentLinks otherLinks
  let numIters = 10
  let dampingFactor = 0.85
  print $ getInputForPageRank mappedLinksButDifferent markedLinks
  -- v txt mame ulozeny graf - indexy stranok vo formate "'odkial' 'kam'" 
--  inputFile <- readFile "data/input2.txt"
  -- na vstupe mame inputFile, pocet iteracii a damping factor nastaveny ako konstantu 0.85
  --writeFile "data/output2.txt" $ show $ startPagerank inputFile numIters dampingFactor 


searchEngineModule :: IO ()
searchEngineModule = do
    let decoded = loadData "data/data.json"
    searchEngine decoded

