{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson (encode)
import qualified Data.ByteString.Char8 as BS (pack)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack, ByteString)
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as FP (decodeString)
import qualified Money as M (transactions)
import Network.HTTP.Types (ok200, movedPermanently301)
import Network.HTTP.Types.Header (hContentType, hLocation)
import Network.Wai (Request, Response, responseLBS, pathInfo)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Util ((&), (>>>))

responseString :: String -> Response
responseString = LBS.pack >>> responseLBS ok200 [(hContentType, "text/plain")]

responseJSON :: LBS.ByteString -> Response
responseJSON = responseLBS ok200 [(hContentType, "application/json")]

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
    | otherwise = notFound

redirect :: String -> Request -> IO Response
redirect url _ = return $ responseLBS movedPermanently301 [(hLocation, BS.pack url)] ""

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ (show $ pathInfo req)

-- assumes request pathInfo is non-empty. Verified since its checked in route. 
-- It would be nice if the compiler could make that guarantee.
static :: Request -> IO Response
static req = a (req { pathInfo = tail (pathInfo req) })
    where
        a = staticApp $ defaultWebAppSettings $ FP.decodeString "./static"

transactions :: Request -> IO Response
transactions _ = M.transactions >>= (encode >>> return) >>= (responseJSON >>> return)
