{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Avoid lambda" #-}

module Parser
    ( something,
    parser,
    ) where

import Text.HTML.TagSoup ( parseTags, innerText, Tag (TagText), isTagOpen, fromTagText, isTagText, maybeTagText, isTagOpenName, isTagCloseName, escapeHTML, (~/=), sections, partitions, (~==), TagRep )
import Text.StringLike
import Data.Maybe (catMaybes)
import Data.List (isInfixOf)
import Data.Tree (flatten)
import Data.Text (splitOn)
import Text.HTML.TagSoup.Match (tagText)

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

parser :: String -> [String]
parser html = do
    let textElementTags = [ "<h1>"  ,"<h2>" ,"<h3>",
                            "<h4>"  ,"<h5>" ,"<h6>",
                            "<p>"   ,"<i>"  ,"<em>",
                            "<span>","<a>"  ,"<div>"]

    let fromBody = dropWhile (~/= "<body>") . takeWhile (~/= "</body>")
    let tags = fromBody $ parseTags html

    rmDup $ concatMap(\s -> concatMap(\s2 -> words s2)s) $ filter(/=[]) $ map(\s -> filter(/= " ") $ getTagText s tags)textElementTags

something :: IO ()
something = do
    --print $ innerText  $ parseTags "<h1>It Works</h1>"
    --print $ parseTags "<h1>It Works</h1><h1>Nice</h1>"
    --print $ parser "<h1>It Works</h1><h1>Nice</h1><script> isHereText ?</script>"
    print "xd"
    -- for debug
    --parser "<script>isHereText?</script><body><h1>It Works</h1><body><h1>It Works</h1><script>isHereText?</script><h2>Nice2</h2><h2>Nice2</h2></body><script>isHereText?</script>"