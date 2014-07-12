{-# LANGUAGE OverloadedStrings #-}
module Util
    ( (?)
    , index
    , maybeRead
    , listApp
    , window
    , substr
    , count
    , mean
    , median
    , splitOnIndices
    , responseLBS'
    , responseHtml
    , responseString
    , responseJSON
    , redirect
    , jsonApp
    , stringToBase64
    , base64ToString
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, pack)
import Data.ByteString.Base64 as B64 (encode, decodeLenient)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.List (foldl', sort)
import Data.Maybe (listToMaybe)
import Data.Monoid (mconcat)
import Network.HTTP.Types (ok200, movedPermanently301, internalServerError500)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai (Request, Response, requestMethod, requestBody, responseLBS)
import Text.JSON (JSON, Result(..), decodeStrict, encodeStrict)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

index :: [a] -> Int -> Maybe a
index [] _ = Nothing
index (x:_) 0 = Just x
index (_:xs) n = index xs (n - 1)

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

-- this eats elements to make sure the output has elements all of length n
window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else take n xs:window n (tail xs)

substr :: Int -> Int -> [a] -> [a]
substr l u = take (u - l) . drop l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

mean :: Floating a => [a] -> a
mean = fst . foldl' (\(m, n) x -> (m+(x-m)/(n+1),n+1)) (0,0)

median :: (Floating a, Ord a) => [a] -> a
median x | odd n  = head  $ drop (n `div` 2) x'
         | even n = mean $ take 2 $ drop i x'
         | otherwise = undefined -- This never happens but not including this case causes non-exhaustive warning. I wonder if this can be solved in dependently typed languages.
                  where i = (length x' `div` 2) - 1
                        x' = sort x
                        n  = length x

--TODO is should be a Set Int not a [Int]
--TODO flip some burgers and get rid of the lambda
-- works like Data.List.Split.splitOn, eats the elements at the indices to split on
splitOnIndices :: [Int] -> [a] -> [[a]]
splitOnIndices is xs = filter (not . null) $ mapTail (drop (1 :: Int)) $ map (\a -> listApp substr a xs) $ window 2 is'
    where
        is' = 0:is ++ [length xs]

base64ToString :: String -> String
base64ToString = BS.unpack . B64.decodeLenient . BS.pack

stringToBase64 :: String -> String
stringToBase64 = BS.unpack . B64.encode . BS.pack

rawRequestBody :: Request -> IO BS.ByteString
rawRequestBody req = mconcat <$> (requestBody req $$ consume)

jsonData :: JSON a => Request -> IO (Result a)
jsonData req
    | requestMethod req == "POST" = (decodeStrict . BS.unpack) <$> rawRequestBody req
    | otherwise = return (Error "Not POST")

responseLBS' :: BS.ByteString -> LBS.ByteString -> Response
responseLBS' c = responseLBS ok200 [(hContentType, c)]

responseHtml :: String -> Response
responseHtml = responseLBS ok200 [(hContentType, "text/html")] . LBS.pack

responseError :: String -> Response
responseError = responseLBS internalServerError500 [(hContentType, "text/plain")] . LBS.pack

responseString :: String -> Response
responseString = responseLBS' "text/plain" . LBS.pack

responseJSON :: JSON a => a -> Response
responseJSON = responseLBS' "application/json" . LBS.pack . encodeStrict

redirect :: String -> Request -> IO Response
redirect url _ = return $ responseLBS movedPermanently301 [(hLocation, BS.pack url)] ""

result :: (String -> b) -> (a -> b) -> Result a -> b
result _ f (Ok a) = f a
result f _ (Error s) = f s

jsonApp :: (JSON a, JSON b) => (a -> IO b) -> Request -> IO Response
jsonApp f req = jsonData req >>= result (return . responseError) (fmap responseJSON . f)
