{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Comuni
  ( Comune(..)
  , readComuni
  ) where

import Prelude.Compat

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.Aeson.Types
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)
import System.Directory (getCurrentDirectory)

data Comune =
  Comune
    { nome :: String
    , codice :: String
    , zona :: Zona
    , regione :: Regione
    , provincia :: Provincia
    , sigla :: String
    , codiceCatastale :: String
    , cap :: [String]
    , popolazione :: Int
    }
  deriving (Show, Generic)

data Zona =
  Zona
    { z_codice :: String
    , z_nome :: String
    }
  deriving (Show, Generic)

data Regione =
  Regione
    { r_codice :: String
    , r_nome :: String
    }
  deriving (Show, Generic)

data Provincia =
  Provincia
    { p_codice :: String
    , p_nome :: String
    }
  deriving (Show, Generic)

instance FromJSON Zona where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance FromJSON Regione where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance FromJSON Provincia where
  parseJSON = genericParseJSON defaultOptions {fieldLabelModifier = drop 2}

instance FromJSON Comune

instance ToJSON Zona where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Regione where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Provincia where
  toJSON = genericToJSON defaultOptions {fieldLabelModifier = drop 2}

instance ToJSON Comune

readComuni :: IO (Maybe [Comune])
readComuni = do
      dir <- getCurrentDirectory
      content <- BL.readFile (dir ++ "/data/comuni.json")
      return (decode content :: Maybe [Comune])
      