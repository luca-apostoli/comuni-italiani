{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Comuni (Comune(..), readComune) where

import Prelude.Compat

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics (Generic)

-- To decode or encode a value using the generic machinery, we must
-- make the type an instance of the Generic class.
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
    { codice :: String
    , nome :: String
    }
  deriving (Show, Generic)

data Regione =
  Regione
    { codice :: String
    , nome :: String
    }
  deriving (Show, Generic)

data Provincia =
  Provincia
    { codice :: String
    , nome :: String
    }
  deriving (Show, Generic)


instance FromJSON Comune
instance ToJSON Comune


readComuni :: Maybe Comune
readComuni =
  decode
    "{\"nome\":\"Abano Terme\",\"codice\":\"028001\",\"zona\":{\"codice\":\"2\",\"nome\":\"Nord-est\"},\"regione\":{\"codice\":\"05\",\"nome\":\"Veneto\"},\"provincia\":{\"codice\":\"028\",\"nome\":\"Padova\"},\"sigla\":\"PD\",\"codiceCatastale\":\"A001\",\"cap\":[\"35031\"],\"popolazione\":19349}" :: Maybe Comune
