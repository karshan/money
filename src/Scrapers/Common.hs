{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Scrapers.Common
    ( Cred (..)
    , Credential (..)
    , queryParamsFromUrl
    ) where

import           Data.Aeson      (FromJSON, ToJSON)
import           Data.List.Split (splitOn)
import           Data.List.Util  ((!!))
import           Data.Maybe      (mapMaybe)
import           GHC.Generics    (Generic)
import           Prelude         hiding (last, (!!))


-- Aeson.encode $ BankOfAmericaCreds $ Cred "hi" "there" [("a","b")] =
-- {
--   "tag": "BankOfAmericaCreds",
--   "contents": {
--     "username":"hi",
--     "password":"there",
--     "secretQuestionAnswers": [["a","b"]]
--   }
-- }
data Credential = BankOfAmericaCreds Cred | ChaseCreds Cred deriving (Generic, FromJSON, ToJSON)

data Cred = Cred { username              :: String
                 , password              :: String
                 , secretQuestionAnswers :: [(String, String)]
                 } deriving (Generic, ToJSON, FromJSON)

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl url =
    mapMaybe parseKeyVal . splitOn "&" $ concat $ tail (splitOn "?" url)
      where
        parseKeyVal :: String -> Maybe (String, String)
        parseKeyVal paramString =
          let xs = splitOn "=" paramString in
          (,) <$> xs !! 0 <*> xs !! 1
