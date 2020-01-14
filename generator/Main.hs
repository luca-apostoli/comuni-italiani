{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Elm.Derive   (defaultOptions, deriveElmDef)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions, ElmOptions(..), UrlPrefix(..),
                               generateElmModuleWith)

import Lib (API, Comune, Zona, Regione, Provincia)

deriveElmDef defaultOptions ''Comune
deriveElmDef defaultOptions ''Zona
deriveElmDef defaultOptions ''Regione
deriveElmDef defaultOptions ''Provincia

getElmOptions :: ElmOptions
getElmOptions = 
    let 
      opt = defElmOptions
      updateOpt x = x { urlPrefix = Static "http://localhost:8080" }
    in
      updateOpt opt 

main :: IO ()
main =
  generateElmModuleWith
    getElmOptions
    [ "Generated"
    , "ComuniApi"
    ]
    defElmImports
    "elm-client/comuni-client/src"
    [ DefineElm (Proxy :: Proxy Comune)
    , DefineElm (Proxy :: Proxy Zona)
    , DefineElm (Proxy :: Proxy Regione)
    , DefineElm (Proxy :: Proxy Provincia)
    ]
    (Proxy :: Proxy API)