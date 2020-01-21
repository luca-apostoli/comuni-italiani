{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T (Text, pack)
import           Elm.Derive   (defaultOptions, deriveElmDef)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, ElmOptions(..), UrlPrefix(..),
                               generateElmModuleWith)

import Lib (API, Comune, Zona, Regione, Provincia, Positive)

deriveElmDef defaultOptions ''Comune
deriveElmDef defaultOptions ''Zona
deriveElmDef defaultOptions ''Regione
deriveElmDef defaultOptions ''Provincia
deriveElmDef defaultOptions ''Positive

getElmOptions :: T.Text -> ElmOptions
getElmOptions endpoint = 
    let 
      opt = defElmOptions
      updateOpt x = x { urlPrefix = Static endpoint }
    in
      updateOpt opt 

main :: IO ()
main = do 
      endpoint <- lookupEnv "ENDPOINT"
      generateElmModuleWith
        (getElmOptions . T.pack $ fromMaybe "http://localhost:8080" endpoint )
        [ "Generated"
        , "ComuniApi"
        ]
        defElmImports
        "elm-client/comuni-client/src"
        [ DefineElm (Proxy :: Proxy Comune)
        , DefineElm (Proxy :: Proxy Zona)
        , DefineElm (Proxy :: Proxy Regione)
        , DefineElm (Proxy :: Proxy Provincia)
        , DefineElm (Proxy :: Proxy Positive)
        ]
        (Proxy :: Proxy API)