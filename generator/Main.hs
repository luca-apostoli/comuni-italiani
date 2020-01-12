{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import           Elm.Derive   (defaultOptions, deriveElmDef)

import           Servant.API  ((:>), Capture, Get, JSON)
import           Servant.Elm  (DefineElm (DefineElm), Proxy (Proxy), defElmImports, defElmOptions,
                               generateElmModuleWith)

import Lib (API, Comune(..), Zona(..), Regione(..), Provincia(..))

deriveElmDef defaultOptions ''Comune
deriveElmDef defaultOptions ''Zona
deriveElmDef defaultOptions ''Regione
deriveElmDef defaultOptions ''Provincia

main :: IO ()
main =
  generateElmModuleWith
    defElmOptions
    [ "Generated"
    , "ComuniApi"
    ]
    defElmImports
    "elm-client"
    [ DefineElm (Proxy :: Proxy Comune)
    , DefineElm (Proxy :: Proxy Zona)
    , DefineElm (Proxy :: Proxy Regione)
    , DefineElm (Proxy :: Proxy Provincia)
    ]
    (Proxy :: Proxy API)