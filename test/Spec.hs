{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Lib (app, Comune(..), listOfFilters)
import Data.Aeson (decode, encode)
import Test.Hspec
import Test.Hspec.Wai
import Test.Hspec.Wai.JSON
import Test.Hspec.Wai.Matcher
import Network.HTTP.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = with (return app) $ do
    describe "GET /comuni" $ do
        it "responds with 200" $ do
            get "/comuni" `shouldRespondWith` 200
        it "should filter response when query is passed" $ do
            get "/comuni?q=20124" `shouldRespondWith` 200 { matchBody = MatchBody (bodyMatcher "20124") }
        it "should return empty response when filter is not found" $ do
            get "/comuni?q=not-a-knwon-query" `shouldRespondWith` 404
    describe "GET /comune/:codice " $ do
        it "responds with 404 when codice is not present" $ do
            get "/comune/fail" `shouldRespondWith` 404
        it "responds with 200 when codice is present" $ do
            get "/comune/028001" `shouldRespondWith` 200
        

bodyMatcher :: String -> [Network.HTTP.Types.Header] -> Body -> Maybe String
bodyMatcher query _ body = case (decode body :: Maybe [Comune]) of
  Just val | length val == length (filter (or . listOfFilters query) val) -> Nothing
  _ -> Just $ "Error in response for comuni, filtered with query " ++ query