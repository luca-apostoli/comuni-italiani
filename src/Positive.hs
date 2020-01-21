{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Positive (Positive, toPositive, fromPositive) where

import GHC.Generics (Generic)
import Web.HttpApiData (FromHttpApiData(..))
import Data.Text (Text, pack)
import qualified BasicPrelude as BP (read)

newtype Positive = Positive {
    unPositive :: Int
} deriving (Eq, Ord, Show, Num, Generic)

toPositive :: Int -> Either Text Positive
toPositive n | n <= 0 = Left $ pack ("Unexpected non-positive integer: " ++ show n)
             | otherwise = Right (Positive n)

fromPositive :: Positive -> Int
fromPositive = unPositive

instance FromHttpApiData Positive where
    parseQueryParam = toPositive . (BP.read :: Text -> Int)