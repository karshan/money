module Scrapers.Common
    ( Cred (..)
    , Credential (..)
    , showCredential
    , queryParamsFromUrl
    ) where

import           Data.List.Split (splitOn)
import           Data.List.Util  ((!!))
import           Data.Maybe      (mapMaybe)
import           Prelude         hiding (last, (!!))
import           Money.API       (Cred (..), Credential (..))


-- Aeson.encode $ BankOfAmericaCreds $ Cred "hi" "there" [("a","b")] =
-- {
--   "tag": "BankOfAmericaCreds",
--   "contents": {
--     "username":"hi",
--     "password":"there",
--     "secretQuestionAnswers": [["a","b"]]
--   }
-- }

showCredential :: Credential -> (String, String)
showCredential (BankOfAmericaCreds c) = ("BankOfAmerica", username c)
showCredential (ChaseCreds c)         = ("Chase"        , username c)

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl url =
    mapMaybe parseKeyVal . splitOn "&" $ concat $ tail (splitOn "?" url)
      where
        parseKeyVal :: String -> Maybe (String, String)
        parseKeyVal paramString =
          let xs = splitOn "=" paramString in
          (,) <$> xs !! 0 <*> xs !! 1
