{-# LANGUAGE OverloadedStrings #-}
module Util
    ( (<&>)
    , (>>>)
    , (&)
    , (?)
    , maybeRead
    , listApp
    , window
    , substr
    , count
    , splitOnIndices
    , jsonData
    , responseString
    , responseJSON
    , redirect
    ) where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Char8 as BS (ByteString, unpack, pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, pack)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.Maybe (listToMaybe)
import Data.Monoid (mconcat)
import Network.HTTP.Types (ok200, movedPermanently301)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai (Request, Response, requestBody, responseLBS)
import Text.JSON (JSON, Result, decodeStrict, encodeStrict)

(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip fmap

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) = flip (.)

--TODO rename to |>
--TODO pick one $ or & don't use both....
(&) :: a -> (a -> b) -> b
(&) = flip ($)

(?) :: Bool -> a -> a -> a
(?) True  x _ = x
(?) False _ y = y

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads

listApp :: (a -> a -> b) -> [a] -> b
listApp f ls = f (head ls) (last ls)

mapTail :: (a -> a) -> [a] -> [a]
mapTail f xs = head xs:map f (tail xs)

window :: Int -> [a] -> [[a]]
window _ [] = []
window n xs = if length xs < n then [] else take n xs:window n (tail xs)

substr :: Int -> Int -> [a] -> [a]
substr l u = take (u - l) . drop l

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

--TODO flip some burgers and get rid of the lambda
splitOnIndices :: [Int] -> [a] -> [[a]]
splitOnIndices is xs = window 2 is' & map (\a -> listApp substr a xs) & mapTail (drop (1 :: Int))
    where
        is' = 0:is ++ [length xs]

rawRequestBody :: Request -> IO BS.ByteString
rawRequestBody req = mconcat <$> (requestBody req $$ consume)

jsonData :: JSON a => Request -> IO (Result a)
jsonData req = rawRequestBody req <&> (BS.unpack >>> decodeStrict)

responseLBS' :: BS.ByteString -> LBS.ByteString -> Response
responseLBS' c = responseLBS ok200 [(hContentType, c)]

responseString :: String -> Response
responseString = LBS.pack >>> responseLBS' "text/plain"

responseJSON :: JSON a => a -> Response
responseJSON = encodeStrict >>> LBS.pack >>> responseLBS' "application/json"

redirect :: String -> Request -> IO Response
redirect url _ = return $ responseLBS movedPermanently301 [(hLocation, BS.pack url)] ""