{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( startApp
  , app
  , API
  , module Comuni
  , findComune
  , listOfFilters
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe (fromMaybe)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
import Data.Char (toLower)
import Data.List (isSubsequenceOf)

import Comuni

type API
   = "comuni" :> QueryParam "q" String :> Get '[ JSON] [Comune] :<|> "comune" :> Capture "codice" String :> Get '[ JSON] Comune

startApp :: IO ()
startApp = run 8080 $ simpleCors app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = listComuni :<|> findComune

findComune :: String -> Handler Comune
findComune codiceComune = do
  listaComuni <- liftIO readComuni
  let maybeCom =
        lookup codiceComune $
        map ((,) <$> codice <*> id) (fromMaybe [] listaComuni)
  case maybeCom of
    Nothing -> throwError err404
    Just comune -> return comune

listComuni :: Maybe String -> Handler [Comune]
listComuni q = do
  listaComuni <- liftIO readComuni
  case listaComuni of
    Nothing -> throwError err503
    Just lista -> do
      let filtered = filterComuni q lista
      if null filtered
        then throwError err404
        else return filtered

filterComuni :: Maybe String -> [Comune] -> [Comune]
filterComuni Nothing lista = lista
filterComuni (Just q) lista = filter (or . listOfFilters q) lista

listOfFilters :: String -> Comune -> [Bool]
listOfFilters q c = map (isSubsequenceOf (lowerStr q) . lowerStr) $ cap c ++ [nome c, codice c, r_nome (regione c), p_nome (provincia c)]

lowerStr :: String -> String
lowerStr = map toLower
