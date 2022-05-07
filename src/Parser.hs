{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Parser
    ( something,
    linkParser,
    linkHtmlParser,
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


parser :: String -> [String]
parser html = do
    let textElementTags = [ "<h1>"  ,"<h2>" ,"<h3>",
                            "<h4>"  ,"<h5>" ,"<h6>",
                            "<p>"   ,"<i>"  ,"<em>",
                            "<span>","<a>"  ,"<div>"]

    let fromBody = dropWhile (~/= "<body>") . takeWhile (~/= "</body>")
    let tags = fromBody $ parseTags html

    rmDup $ concatMap(\s -> concatMap(\s2 -> words s2)s) $ filter(/=[]) $ map(\s -> filter(/= " ") $ getTagText s tags)textElementTags


checkIfIndex2Exist :: Foldable t => t a -> Bool
checkIfIndex2Exist array = do
    length array > 3

linkHtmlParser :: String -> [String]
linkHtmlParser html = do
    let textElementTags = ["<a>"]

    let fromBody = dropWhile (~/= "<body>") . takeWhile (~/= "</body>")
    let tags = fromBody $ parseTags html

    filter(/= " ") $  map(\s -> if checkIfIndex2Exist s then s!!2 else " ") $ concatMap(\s -> map( \s2 -> Data.List.Split.splitOn "/" s2) s) $ filter(/=[]) $ map(\s -> filter(/= " ") $ getHrefLink s tags)textElementTags

linkParser :: [String] -> [String]
linkParser link = do
    map(\s -> if checkIfIndex2Exist s then s!!2 else " ") $ map(\s ->  Data.List.Split.splitOn "/" s) link

something :: IO ()
something = do
    --print $ innerText  $ parseTags "<h1>It Works</h1>"
    --print $ parseTags "<h1>It Works</h1><h1>Nice</h1>"
    --print $ parser "<h1>It Works</h1><h1>Nice</h1><script> isHereText ?</script>"
    --print "xd"
    -- for debug
    print $ linkHtmlParser "<script>isHereText?</script><body><h1>It Works</h1><body><h1>It Works</h1><a href='got it'></a><script>isHereText?</script><h2>Nice2</h2><h2>Nice2</h2></body><script>isHereText?</script>"