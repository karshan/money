{-# LANGUAGE OverloadedStrings #-}
module Main where

import BoaScrape (getLatestTransactions)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever)
import Data.List (deleteFirstsBy)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified ManageDB as MDB (updateTransactions)
import qualified Money as M (Transaction(..))
import Network.HTTP.Types.Status (ok200)
import Network.Wai (Application, pathInfo, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Locale (defaultTimeLocale)

main :: IO ()
main =  do
    _ <- forkIO updateThread
    putStrLn ("Listening on port " ++ show port)
    run port app
    where
        port = 3000

mergeTransactions :: [M.Transaction] -> [M.Transaction] -> [M.Transaction]
mergeTransactions new old = old ++ deleteFirstsBy f new old
    where
        f a b = M.date a == M.date b && M.description a == M.description b && M.amount a == M.amount b

updateThread :: IO ()
updateThread = forever $ do
    new <- getLatestTransactions
    void $ MDB.updateTransactions (mergeTransactions new)
    now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> getCurrentTime
    putStrLn $ "updated transactions " ++ now
    threadDelay $ periodInSeconds * (10 ^ (6 :: Int))
        where
            periodInSeconds = 60

app :: Application
app req = route (pathInfo req) req

--route :: [Text] -> Request -> IO Response
route path
    | null path = notFound
    | path == ["transaction"] = notFound
    | otherwise = notFound

notFound :: Application
notFound req continuation = continuation $ responseLBS ok200 [] $ "not found" <> (fromString $ show $ pathInfo req) --return $ responseString $ "not found " ++ show (pathInfo req)