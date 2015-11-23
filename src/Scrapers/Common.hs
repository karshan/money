module Scrapers.Common
    ( queryParamsFromUrl
    ) where

import           Data.List.Split (splitOn)
import           Data.List.Util  (last, (!!))
import           Data.Maybe      (fromMaybe, mapMaybe)
import           Prelude         hiding (last, (!!))

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl url =
  fromMaybe [] (mapMaybe parseKeyVal . splitOn "&;" <$> last (splitOn "?" url))
    where
      parseKeyVal :: String -> Maybe (String, String)
      parseKeyVal paramString =
        let xs = splitOn "=" paramString in
        (,) <$> xs !! 0 <*> xs !! 1
