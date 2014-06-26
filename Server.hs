{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.List ((\\))
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as FP (decodeString)
import qualified ManageDB as MDB (getTransactions, addTransaction, deleteTransaction, updateTransaction, updateTransactions)
import qualified Money as M (Transaction, tags, similarTransactions)
import Network.HTTP.Types.Header (hContentLength)
import Network.Wai (Request, Response, pathInfo)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util (mapHeaders)
import Text.JSON (Result(..), JSValue(..), JSON(..), toJSObject, fromJSObject, toJSString, fromJSString)
import Util (jsonApp, redirect, responseJSON, responseString)

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
    | path == ["delete"] = deleteTransaction
    | path == ["add"] = addTransaction
    | path == ["update"] = updateTransaction
    | path == ["updateTags"] = updateTags
    | otherwise = notFound

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ (show $ pathInfo req)

-- assumes request pathInfo is non-empty. Verified since its checked in route.
-- It would be nice if the compiler could make that guarantee.
static :: Request -> IO Response
static req = fmap (mapHeaders (filter (\(k,_) -> k /= hContentLength))) $ a (req { pathInfo = tail (pathInfo req) })
    where
        a = staticApp $ defaultWebAppSettings $ FP.decodeString "./static"

-- TODO error reporting
ts :: IO [M.Transaction]
ts = fromMaybe [] <$> MDB.getTransactions

transactions :: Request -> IO Response
transactions _ = fmap responseJSON ts

similarTransactions :: Request -> IO Response
similarTransactions = jsonApp (\t -> M.similarTransactions t <$> ts)

-- TODO error reporting
deleteTransaction :: Request -> IO Response
deleteTransaction = jsonApp (void . MDB.deleteTransaction)

-- TODO error reporting
addTransaction :: Request -> IO Response
addTransaction = jsonApp (void . MDB.addTransaction)

-- TODO error reporting
updateTransaction :: Request -> IO Response
updateTransaction = jsonApp (\a -> if length a == 2 then void $ MDB.updateTransaction (a !! 0) (a !! 1) else return ())

data UpdateTags = UpdateTags { ut_ts :: [M.Transaction], ut_tags :: String }

instance JSON UpdateTags where
  readJSON (JSObject o) = let l = fromJSObject o in
                              do _ts <- jslookup "ts" l
                                 _tags <- jslookup "tags" l
                                 return UpdateTags { ut_ts = _ts, ut_tags = _tags }
                                   where
                                     jslookup k l = maybe (Error $ "missing key: " ++ k) readJSON (lookup k l)
  readJSON _ = Error "Not an object"
  showJSON = undefined

updateTags :: Request -> IO Response
updateTags = jsonApp (\a -> void $ MDB.updateTransactions (\allts -> (allts \\ ut_ts a) ++ map (\t -> t { M.tags = splitOn " " (ut_tags a) }) (ut_ts a)))
