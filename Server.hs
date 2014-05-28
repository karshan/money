{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (void)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Filesystem.Path.CurrentOS as FP (decodeString)
import qualified ManageDB as MDB (getTransactions, addTransaction, deleteTransaction)
import qualified Money as M (Transaction, similarTransactions)
import Network.HTTP.Types.Header (hContentLength)
import Network.Wai (Request, Response, pathInfo)
import Network.Wai.Application.Static (staticApp, defaultWebAppSettings)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Util (mapHeaders)
import Util ((<&>), jsonApp, redirect, responseJSON, responseString)

main :: IO ()
main = putStrLn ("Listening on port " ++ show port) >> run port app
    where
        port = 3000

app :: Request -> IO Response
app req = route (pathInfo req) $ req

route :: [Text] -> Request -> IO Response
route path
    | null path = redirect "/static/build/Main.html"
    | head path == "static" = static
    | path == ["transactions"] = transactions
    | path == ["similar"] = similarTransactions
    | path == ["delete"] = deleteTransaction
    | path == ["add"] = addTransaction
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
ts = MDB.getTransactions <&> fromMaybe []

transactions :: Request -> IO Response
transactions _ = fmap responseJSON ts

similarTransactions :: Request -> IO Response
similarTransactions = jsonApp (\t -> M.similarTransactions <$> ts <*> return t)

-- TODO error reporting
deleteTransaction :: Request -> IO Response
deleteTransaction = jsonApp (void . MDB.deleteTransaction)

-- TODO error reporting
addTransaction :: Request -> IO Response
addTransaction = jsonApp (void . MDB.addTransaction)
