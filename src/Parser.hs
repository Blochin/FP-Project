{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser
    ( something,
    parser
    ) where

import Text.HTML.TagSoup ( parseTags, innerText, Tag (TagText), isTagOpen, fromTagText, isTagText, maybeTagText, isTagOpenName, isTagCloseName, escapeHTML, (~/=), sections, partitions )
import Text.StringLike
import Data.Maybe (catMaybes)
import Data.List (isInfixOf)

parser :: String -> [String]
parser html = do
    let fromBody = takeWhile (~/= "</body>")  . dropWhile (~/= "<body>")
    let parsedTags = parseTags html
    let bodyTags = fromBody parsedTags
    --let filterTags =  takeWhile(~/= "<script>") .dropWhile (~/= "</script>")
    --let filteredTags = filterTags parsedTags
    let mybTagText = map maybeTagText bodyTags
    filter(/= " ") $ catMaybes mybTagText

something :: IO ()
something = do
    --print $ innerText  $ parseTags "<h1>It Works</h1>"
    --print $ parseTags "<h1>It Works</h1><h1>Nice</h1>"
    --print $ parser "<h1>It Works</h1><h1>Nice</h1><script> isHereText ?</script>"
    
    -- for debug
    print $ parser "<script>isHereText?</script><body><h1>It Works</h1><body><h1>It Works</h1><script>isHereText?</script><h1>Nice</h1><h1>Nice</h1></body><script>isHereText?</script>" 