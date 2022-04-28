{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module PageData
    ( PageData(..)
    ) where

import Data.Text ( Text, unpack )
import qualified Control.Applicative
import Data.Aeson

data PageData =
  PageData { url  :: !Text
           , html_content   :: !Text
           } deriving (Show)

instance FromJSON PageData where
  parseJSON (Object v) = do
    url <- v .: "url"
    html_content <- v .: "html_content"
    return (PageData {url = url, html_content = html_content})
  parseJSON _ = Control.Applicative.empty

getUrl :: Maybe PageData -> String
getUrl (Just pageData) = unpack $ url pageData
getUrl Nothing = undefined

getHtml :: Maybe PageData -> String
getHtml (Just pageData) = unpack $ html_content pageData
getHtml Nothing = undefined