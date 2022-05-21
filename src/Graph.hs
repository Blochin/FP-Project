{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Graph
    ( createGraph,
    markLinks,
    getAllIndexes,
    getInputForPageRank
    ) where

import Data.Map ( insert )
import Data.Char ()
import Data.List ( nubBy, find, nub )
import Parser ( linkHtmlParser, linkParser )
import Loader (getUrl, getHtml)
import PageData ( PageData )
import Data.Tuple.Select ( Sel2(sel2), Sel1 (sel1) )

printTouples :: (Show a, Show b) => (a, b) -> [Char]
printTouples (index, uid) = "index: " ++ show index ++ " page: " ++ show uid

rmDup :: (Eq a) => [(a, b)] -> [(a, b)]
rmDup lst = go lst []
    where go [] seen = seen
          go (x:xs) seen
              | any (\(a, _) -> a == fst x) seen = go xs seen
              | otherwise = go xs (seen ++ [x])

arrayRmDup :: Eq a => [a] -> [a]
arrayRmDup [] = []
arrayRmDup (x:xs) = x : arrayRmDup (filter (\y -> not(x == y)) xs)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys


createIndexPageTouple :: Eq a => [a] -> [(a, Int)]
createIndexPageTouple links = do
    let indexes = [0.. length links-1]
    --nubBy (\(x,_) (x', _) -> x == x') $ zip  links indexes
    rmDup $ zip  links indexes

markLinks :: (Eq a, Foldable t) => [a] -> t [a] -> [(a, Int)]
markLinks currentLinks otherLinks = do
    let mergedOtherLinks = concat otherLinks
    let allLinks = arrayRmDup $ merge currentLinks mergedOtherLinks
    createIndexPageTouple allLinks

translateUrl :: (Foldable t, Eq a1, Sel1 a2 a1) => a1 -> t a2 -> Maybe a2
translateUrl url urlArray = do
    find(\s -> sel1 s == url )urlArray

getIndex :: (Show a1, Ord a1, Eq p, Sel1 a2 p, Sel2 a2 a1, Num a1) => [a2] -> p -> String
getIndex allLinksWithIndex word = do
    let index = map (\s -> (if word == sel1 s then sel2 s else -1)) allLinksWithIndex
    show $ maximum index
    
getAllIndexes :: (Sel1 a p, Sel1 a2 p, Show a1, Ord a1, Num a1, Sel2 a2 a1, Sel2 a p, Eq p) => [[a]] -> [a2] -> [[(String, String)]]
getAllIndexes mappedLinks markedLinks = do
    map(map (\s -> (getIndex markedLinks (sel1 s),getIndex markedLinks (sel2 s)))) mappedLinks

createGraph :: (Eq a1, Foldable t, Sel1 a2 a1, Sel1 a a1, Sel2 a a1) => [[a]] -> t a2 -> [[(Maybe a2, Maybe a2)]]
createGraph mappedLinks markedLinks = do
    map(\s-> map(\s2->(translateUrl (sel1 s2) markedLinks,translateUrl (sel2 s2) markedLinks))s)mappedLinks

getInputForPageRank :: (Sel1 a p, Sel1 a2 p, Show a1, Ord a1, Num a1, Sel2 a2 a1, Sel2 a p, Eq p) => [[a]] -> [a2] -> [String]
getInputForPageRank mappedLinks markedLinks = do
    let twoDimArray =  map(map(\s -> getIndex markedLinks (sel1 s)  ++ " " ++ getIndex markedLinks (sel2 s))) mappedLinks
    let inputForPageRank = concat  twoDimArray
    nub inputForPageRank