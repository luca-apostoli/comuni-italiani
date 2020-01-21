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
import Positive

type API
   = "comuni" :> QueryParam "q" String :> QueryParam "pos" Positive :> QueryParam "limit" Positive :> Get '[ JSON] [Comune] :<|> "comune" :> Capture "codice" String :> Get '[ JSON] Comune :<|> Get '[ JSON] String

startApp :: Maybe String -> IO ()
startApp (Just port) = run (read port :: Int) $ simpleCors app
startApp _ = run 8080 $ simpleCors app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = listComuni :<|> findComune :<|> healthCheck

healthCheck :: Handler String
healthCheck = return "OK"

findComune :: String -> Handler Comune
findComune codiceComune = do
  listaComuni <- liftIO readComuni
  let maybeCom =
        lookup codiceComune $
        map ((,) <$> codice <*> id) (fromMaybe [] listaComuni)
  case maybeCom of
    Nothing -> throwError err404
    Just comune -> return comune

listComuni :: Maybe String -> Maybe Positive -> Maybe Positive -> Handler [Comune]
listComuni q pos limit = do
  listaComuni <- liftIO readComuni
  case listaComuni of
    Nothing -> throwError err503
    Just lista -> do
      let filtered = filterComuni q lista
      if null filtered
        then throwError err404
        else return $ (truncateList limit . dropList pos) filtered

dropList :: Maybe Positive -> [a] -> [a]
dropList Nothing = id
dropList (Just pos) = drop (fromPositive pos)

truncateList :: Maybe Positive -> [a] -> [a]
truncateList Nothing = id
truncateList (Just limit) = take (fromPositive limit)


filterComuni :: Maybe String -> [Comune] -> [Comune]
filterComuni Nothing lista = lista
filterComuni (Just q) lista = filter (or . listOfFilters q) lista

listOfFilters :: String -> Comune -> [Bool]
listOfFilters q c = map (isSubsequenceOf (lowerStr q) . lowerStr) $ cap c ++ [nome c, codice c, r_nome (regione c), p_nome (provincia c)]

lowerStr :: String -> String
lowerStr = map toLower
