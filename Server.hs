{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS (unpack, pack, ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack, ByteString)
import Data.Conduit (($$))
import Data.Conduit.List (consume)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as FP (decodeString)
import qualified ManageDB as MDB (getTransactions, addTransaction)
import qualified Money as M (transactions)
import Network.HTTP.Types (ok200, movedPermanently301)
import Network.HTTP.Types.Header (hContentType, hLocation, hContentLength)
import Network.Wai (Request, Response, responseLBS, pathInfo, requestMethod, requestBody)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util (mapHeaders)
import Text.JSON (encodeStrict)
import Util ((&), (>>>))

rawRequestBody :: Request -> IO BS.ByteString
rawRequestBody req = mconcat <$> (requestBody req $$ consume)

responseLBS' :: BS.ByteString -> LBS.ByteString -> Response
responseLBS' c = responseLBS ok200 [(hContentType, c)]

responseString :: String -> Response
responseString = LBS.pack >>> responseLBS' "text/plain"

responseJSON :: String -> Response
responseJSON = LBS.pack >>> responseLBS' "application/json"

main :: IO ()
main = putStrLn ("Listening on port " ++ show port) >> run port app
    where
        port = 3000

app :: Request -> IO Response
app req = route (pathInfo req) $ req 

route :: [Text] -> Request -> IO Response
route path
    | null path = redirect "/static/index.html"
    | head path == "static" = static
    | path == ["transactions"] = transactions
    | path == ["similar"] = similarTransactions
    | otherwise = notFound

redirect :: String -> Request -> IO Response
redirect url _ = return $ responseLBS movedPermanently301 [(hLocation, BS.pack url)] ""

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ (show $ pathInfo req)

-- assumes request pathInfo is non-empty. Verified since its checked in route. 
-- It would be nice if the compiler could make that guarantee.
static :: Request -> IO Response
static req = fmap (mapHeaders (filter (\(k,v) -> k /= hContentLength))) $ a (req { pathInfo = tail (pathInfo req) })
    where
        a = staticApp $ defaultWebAppSettings $ FP.decodeString "./static"

transactions :: Request -> IO Response
transactions _ = MDB.getTransactions >>= (fromMaybe [] >>> encodeStrict >>> responseJSON >>> return)

similarTransactions :: Request -> IO Response
similarTransactions req
    | requestMethod req == "POST" = rawRequestBody req >>= (bsToLBS >>> responseLBS' "text/plain" >>> return)
    | otherwise = notFound req
        where
            bsToLBS = BS.unpack >>> LBS.pack
