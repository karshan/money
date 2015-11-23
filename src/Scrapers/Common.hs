module Scrapers.Common
    ( queryParamsFromUrl
    ) where

import           Data.List.Split (splitOn)
import           Data.List.Util  ((!!))
import           Data.Maybe      (mapMaybe)
import           Prelude         hiding (last, (!!))

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl url =
    mapMaybe parseKeyVal . splitOn "&" $ concat $ tail (splitOn "?" url)
      where
        parseKeyVal :: String -> Maybe (String, String)
        parseKeyVal paramString =
          let xs = splitOn "=" paramString in
          (,) <$> xs !! 0 <*> xs !! 1
