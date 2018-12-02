{-# LANGUAGE OverloadedStrings #-}

module MalShare.Internal.Request
  ( getDetails
  , getFile
  , getHashList
  , getHashListRaw
  , getHashesOfType
  , getSources
  , getSourcesRaw
  , getLimit
  , getTypes
  , buildMalShareRequest
  ) where

import Config
import MalShare.Internal.Types

import Control.Lens
import Data.Aeson
import Data.ByteString
import qualified Data.ByteString.Lazy as BL
import Data.Maybe
import qualified Data.Text as T
import Network.Wreq
import Network.HTTP.Simple

base_url = "https://malshare.com/api.php"

buildMalShareRequest :: [(T.Text, T.Text)] -> T.Text
buildMalShareRequest params = (T.pack base_url) <> "?api_key=" <> msapikey <> buildQuery params
  where buildQuery [] = ""
        buildQuery (x:xs) = "&" <> (fst x) <> "=" <> (snd x) <> (buildQuery xs)


getHashList :: IO (Maybe [MalwareHashes])
getHashList = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["getlist"]
  resp <- getWith opts base_url
  return (decode (fromJust $ resp ^? responseBody) :: Maybe [MalwareHashes])

getHashListRaw :: IO (BL.ByteString)
getHashListRaw = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["getlistraw"]
  resp <- getWith opts base_url
  return . fromJust $ resp ^? responseBody

-- Sources is a list of a source for each item in the hash list
getSources :: IO (Maybe [MalwareSource])
getSources = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["getsources"]
  resp <- getWith opts base_url
  return (decode (fromJust $ resp ^? responseBody) :: Maybe [MalwareSource])

getSourcesRaw :: IO (BL.ByteString)
getSourcesRaw = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["getsourcesraw"]
  resp <- getWith opts base_url
  return . fromJust $ resp ^? responseBody

getTypes :: IO (Maybe [MalwareTypes])
getTypes = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["gettypes"]
  resp <- getWith opts base_url
  return (decode (fromJust $ resp ^? responseBody) :: Maybe [MalwareTypes])

getLimit :: IO (BL.ByteString)
getLimit = do
  let opts = defaults & param "api_key" .~ [msapikey] & param "action" .~ ["getlimit"]
  resp <- getWith opts base_url
  return . fromJust $ resp ^? responseBody

getFile :: T.Text -> IO (BL.ByteString)
getFile hash = do
  let opts = defaults
             & param "api_key" .~ [msapikey]
             & param "action" .~ ["getfile"]
             & param "hash" .~ [hash]
  resp <- getWith opts base_url
  return . fromJust $ resp ^? responseBody


getHashesOfType :: T.Text -> IO (Maybe [MalwareHashes])
getHashesOfType p = do
  let opts = defaults
             & param "api_key" .~ [msapikey]
             & param "action" .~ ["type"]
             & param "type" .~ [p]
  resp <- getWith opts base_url
  return (decode (fromJust $ resp ^? responseBody) :: Maybe [MalwareHashes])

getDetails :: T.Text -> IO (Maybe MalwareDetails)
getDetails p = do
  let opts = defaults
             & param "api_key" .~ [msapikey]
             & param "action" .~ ["details"]
             & param "hash" .~ [p]
  resp <- getWith opts base_url
  return (decode (fromJust $ resp ^? responseBody) :: Maybe MalwareDetails)
