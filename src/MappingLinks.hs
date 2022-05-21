{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module MappingLinks
  ( mapLinks
  )
where

import Parser ( linkHtmlParser, oneLinkParser )
import Loader ( getUrl, getHtml )
import Data.Tuple.All ( Sel1(sel1), Sel2(sel2) )
import qualified PageData

mapLinks :: [Maybe PageData.PageData] -> [[(String, String)]]
mapLinks unpackedPages  = do  
    let mappedLinks = map (\s -> (oneLinkParser $ getUrl s, linkHtmlParser $ getHtml s )) unpackedPages
    map(\s-> filter(/= ("","")) $ map(\s2-> if(sel1 s /= s2) then (sel1 s,s2) else ("","")) $ sel2 s) mappedLinks