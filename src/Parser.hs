{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Parser
    (linkParser,
    linkHtmlParser,
    oneLinkParser,
    parser,
    ) where

import Text.HTML.TagSoup ( parseTags, innerText, Tag (TagText), isTagOpen, fromTagText, isTagText, maybeTagText, isTagOpenName, isTagCloseName, escapeHTML, (~/=), sections, partitions, (~==), TagRep, fromAttrib )
import Text.StringLike
import Data.Maybe (catMaybes)
import Data.List (isInfixOf)
import Data.Tree (flatten)
import Data.Text (splitOn)
import Text.HTML.TagSoup.Match (tagText)
import Data.List.Split
import GHC.IO.Encoding
import Text.Regex as R


type Regist = [String]
type ListRe = [Regist]

rmDup :: Eq a => [a] -> [a]
rmDup [] = []
rmDup (x:xs) = x : rmDup (filter (\y -> not(x == y)) xs)

getTagText :: (StringLike b, Text.HTML.TagSoup.TagRep t, Show b) =>t -> [Tag b] -> [b]
getTagText selector tags =
    map f $ sections (~== selector)  tags
    where
        f = fromTagText . head . filter isTagText

getHrefLink :: TagRep t => t -> [Tag String] -> [String]
getHrefLink selector tags =
    map f $ sections (~== selector)  tags
    where
        f = fromAttrib "href"  . head . filter isTagOpen
        

replaceUnicode :: Foldable t => t (String, String) -> String -> String
replaceUnicode unicodeList string = 
    let substringRegex = R.subRegex 
        replaceAllIn = foldl(\acc (k,v)->substringRegex (R.mkRegex k)acc v)
        in replaceAllIn string unicodeList 


mapReplaceUnicode :: [String] -> [String]
mapReplaceUnicode xs  = do
    let unicodesLatinList = [("\224","a"),("\225","a"),("\226","a"),("\227","a"),("\228","a"),("\229","a"),
                            ("\231","c"),
                            ("\232","e"),("\233","e"),("\234","e"),("\235","e"),
                            ("\236","i"),("\237","i"),("\238","i"),("\239","i"),
                            ("\240","o"),
                            ("\241","n"),
                            ("\242","o"),("\243","o"),("\244","o"),("\245","o"),("\246","o"),
                            ("\249","u"),("\250","u"),("\251","u"),
                            ("\253","y"),("\255","y"),
                            
                            ("\192","A"),("\193","A"),("\194","A"),("\195","A"),("\196","A"),("\197","A"),
                            ("\199","C"),
                            ("\200","E"),("\201","E"),("\202","E"),("\203","E"),
                            ("\204","I"),("\205","I"),("\206","I"),("\207","I"),
                            ("\208","D"),
                            ("\209","N"),
                            ("\210","O"),("\211","O"),("\212","O"),("\213","O"),("\214","O"),
                            ("\217","U"),("\218","U"),("\219","U"),
                            ("\221","Y"),
                            ("\222","P"),
                            ("\223","B")]
    map(\x -> replaceUnicode unicodesLatinList x) xs

parser :: String -> [String]
parser html = do
    let textElementTags = [ "<h1>"  ,"<h2>" ,"<h3>",
                            "<h4>"  ,"<h5>" ,"<h6>",
                            "<p>"   ,"<i>" ]


    let fromBody = dropWhile (~/= "<body>") . takeWhile (~/= "</body>")
    let tags = fromBody $ parseTags html

    mapReplaceUnicode $ rmDup $ concatMap(\s -> concatMap(\s2 -> words s2)s) $ filter(/=[]) $ map(\s -> filter(/= " ") $ getTagText s tags)textElementTags

checkIfIndex2Exist :: Foldable t => t a -> Bool
checkIfIndex2Exist array = do
    length array > 3

checkIfIndex0Exist :: Foldable t => t a -> Bool
checkIfIndex0Exist array = do
    length array > 1

linkHtmlParser :: String -> [String]
linkHtmlParser html = do
    let textElementTags = ["<a>"]

    let fromBody = dropWhile (~/= "<body>") . takeWhile (~/= "</body>")
    let tags = fromBody $ parseTags html

    filter(/= " ") $  map(\s -> s!!0) $ concatMap(\s -> map( \s2 -> Data.List.Split.splitOn "?" s2) s) $ filter(/=[]) $ map(\s -> filter(/= " ") $ getHrefLink s tags)textElementTags

linkParser :: [String] -> [String]
linkParser link = do
    map(\s -> s!!0) $ map(\s ->  Data.List.Split.splitOn "?" s) link

oneLinkParser :: String -> String
oneLinkParser link = do
    if checkIfIndex2Exist link
        then (Data.List.Split.splitOn "?"  link)!!0
        else "Can't match with link"
