{-# HLINT ignore "Use id" #-}
{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Redundant bracket" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module InvertedIndex
  ( invertedIndex, invertedIndex2, indexWithPageRank
  )
where

import Data.Tuple.Select (Sel1 (..), Sel2 (..))
import Data.List (nub)
import Data.Map ((!))
import Data.Map.Internal(Map)


invertedIndex :: (Foldable t, Eq a1, Sel1 a2 String, Sel2 a2 [a1]) => a1 -> t a2 -> [(a1, String)]
invertedIndex word mappedWords = do
  map (\s3 -> (word, s3)) $ concatMap (\s -> filter (/= " ") $ map (\s2 -> if word == s2 then sel1 s else " ") $ sel2 s) mappedWords

invertedIndex2 :: (Foldable t1, Sel1 a2 String, Sel2 a2 [a1], Foldable t2, Eq a1) => t2 [a1] -> t1 a2 -> [(a1, [String])]
invertedIndex2 words mappedWords = do
    let vsetkySlova = concat words
    let unikatneSlova = nub vsetkySlova
    map(\keyword-> ((keyword,(map(\s3->(s3)) $ concatMap(\s -> filter(/= " ") $ map(\s2 -> if keyword == s2 then sel1 s else " ") $ sel2 s) mappedWords) ))) unikatneSlova
  
--teraz by to malo byt zoradene vzostupne opacne to neviem dat
-- mapa je v tvare pagerank, link, index linku aby sme si to vedeli overit
indexWithPageRank :: (Ord c, Eq b, Num c) => [b] -> [(b, c)] -> Map c a -> [(a, b, c)]
indexWithPageRank result markedLinks pageRankData  = do
  let unikatneStranky = nub result
  let pagesWithJustIndex = map(\stranka -> (stranka, lookup stranka markedLinks)) unikatneStranky
  let pagesWithIndex = map (\result -> ((sel1 result,case sel2 result of { Nothing -> 0; Just result -> result }))) pagesWithJustIndex
  map(\s -> ((pageRankData ! sel2 s, sel1 s, sel2 s))) pagesWithIndex
  
