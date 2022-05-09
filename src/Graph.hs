{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Graph
    ( createGraph,
    markLinks
    ) where

import Data.Map ( insert )
import Data.Char ()
import Data.List ( nubBy )
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
    let allLinks = merge currentLinks mergedOtherLinks
    createIndexPageTouple allLinks

createGraph :: Sel2 a b => [a] -> [b]
createGraph mappedLinks = do
    map(\s -> sel2  s )mappedLinks

