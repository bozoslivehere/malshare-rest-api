{-# LANGUAGE OverloadedStrings, RecordWildCards, DeriveGeneric #-}

module MalShare.Internal.Types
  ( MalwareHashes(..)
  , MalwareHashList(..)
  , MalwareDetails(..)
  , MalwareSource(..)
  , MalwareTypes(..)
  ) where

import Data.Aeson
import Data.Maybe
import qualified Data.Text as T
import GHC.Generics


data MalwareHashes = MalwareHashes
  { md5 :: String
  , sha1 :: String
  , sha256 :: String
  } deriving (Show, Generic)

instance ToJSON MalwareHashes
instance FromJSON MalwareHashes

newtype MalwareHashList = MalwareHashList { malwareHashList :: [MalwareHashes] } deriving (Show, Generic)

instance ToJSON MalwareHashList
instance FromJSON MalwareHashList

data MalwareSource = MalwareSource (Maybe T.Text) deriving (Generic, Show)
instance FromJSON MalwareSource

data MalwareDetails = MalwareDetails
  { md_md5 :: T.Text
  , md_sha1 :: T.Text
  , md_sha256 :: T.Text
  , md_ssdeep :: T.Text
  , md_f_type :: T.Text
  , md_sources :: Maybe [MalwareSource]
  } deriving (Show, Generic)

instance FromJSON MalwareDetails where
  parseJSON = withObject "details" $ \o -> do
    md_md5 <- o .: "MD5"
    md_sha1 <- o .: "SHA1"
    md_sha256 <- o .: "SHA256"
    md_ssdeep <- o .: "SSDEEP"
    md_f_type <- o .: "F_TYPE"
    md_sources <- o .:? "SOURCES"

    return MalwareDetails{..}

-- TODO: Finish entering all file types (currently top 35 or so)
data MalwareTypes = MalwareTypes
  { mt_8086 :: Maybe Int
  , mt_Android :: Maybe Int
  , mt_ASCII :: Maybe Int
  , mt_BASH :: Maybe Int
  , mt_Compiled :: Maybe Int
  , mt_Composite :: Maybe Int
  , mt_Dalvik :: Maybe Int
  , mt_Data :: Maybe Int
  , mt_DOS :: Maybe Int
  , mt_ELF :: Maybe Int
  , mt_Exported :: Maybe Int
  , mt_GIF :: Maybe Int
  , mt_GZ :: Maybe Int
  , mt_HTML :: Maybe Int
  , mt_ISO8859 :: Maybe Int
  , mt_Java :: Maybe Int
  , mt_JPEG :: Maybe Int
  , mt_Microsoft :: Maybe Int
  , mt_MS :: Maybe Int
  , mt_MSDOS :: Maybe Int
  , mt_NonISO :: Maybe Int
  , mt_PE32 :: Maybe Int
  , mt_PE32Plus :: Maybe Int
  , mt_PHP :: Maybe Int
  , mt_PNG :: Maybe Int
  , mt_POSIX :: Maybe Int
  , mt_RAR :: Maybe Int
  , mt_RTF :: Maybe Int
  , mt_Troff :: Maybe Int
  , mt_UTF8 :: Maybe Int
  , mt_XML :: Maybe Int
  , mt_Zip :: Maybe Int
  , mt_Zlib :: Maybe Int
  , mt_Dash :: Maybe Int
  , mt_Python :: Maybe Int
  , mt_CPlusPlus :: Maybe Int
  } deriving (Show, Generic)

instance FromJSON MalwareTypes where
  parseJSON = withObject "type" $ \o -> do
    mt_8086 <- o .:? "8086"
    mt_Android <- o .:? "Android"
    mt_ASCII <- o .:? "ASCII"
    mt_BASH <- o .:? "Bourne-Again"
    mt_Compiled <- o .:? "compiled"
    mt_Composite <- o .:? "Composite"
    mt_Dalvik <- o .:? "Dalvik"
    mt_Data <- o .:? "data"
    mt_DOS <- o .:? "DOS"
    mt_ELF <- o .:? "ELF"
    mt_Exported <- o .:? "exported"
    mt_GIF <- o .:? "GIF"
    mt_GZ <- o .:? "gzip"
    mt_HTML <- o .:? "HTML"
    mt_ISO8859 <- o .:? "ISO-8859"
    mt_Java <- o .:? "Java"
    mt_JPEG <- o .:? "JPEG"
    mt_Microsoft <- o .:? "Microsoft"
    mt_MS <- o .:? "MS"
    mt_MSDOS <- o .:? "MS-DOS"
    mt_NonISO <- o .:? "Non-ISO"
    mt_PE32 <- o .:? "PE32"
    mt_PE32Plus <- o .:? "PE32+"
    mt_PHP <- o .:? "PHP"
    mt_PNG <- o .:? "PNG"
    mt_POSIX <- o .:? "POSIX"
    mt_RAR <- o .:? "RAR"
    mt_RTF <- o .:? "Rich"
    mt_Troff <- o .:? "troff"
    mt_UTF8 <- o .:? "UTF-8"
    mt_XML <- o .:? "XML"
    mt_Zip <- o .:? "Zip"
    mt_Zlib <- o .:? "zlib"
    mt_Dash <- o .:? "-"
    mt_Python <- o .:? "Python"
    mt_CPlusPlus <- o .:? "C++"


    return MalwareTypes{..}
