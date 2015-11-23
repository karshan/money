{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent        (forkIO, threadDelay)
import           Control.Monad             (forever, void)
import           Data.Function             (on)
import           Data.List                 (deleteFirstsBy)
import           Data.Maybe                (fromMaybe)
import           Data.Monoid               ((<>))
import           Data.String               (IsString, fromString)
import           Data.Time.Clock           (getCurrentTime)
import           Data.Time.Format          (defaultTimeLocale, formatTime)
import qualified ManageDB                  as MDB (updateTransactions, getTransactions)
import qualified Money                     as M (Transaction (..))
import           Network.HTTP.Types.Status (ok200)
import           Network.Wai               (Application, pathInfo, responseLBS)
import           Network.Wai.Handler.Warp  (run)
import           Scrapers.BankOfAmerica    (getLatestTransactions)
import qualified Text.JSON                 as JSON (encode)

main :: IO ()
main = do
    _ <- forkIO updateThread
    putStrLn ("Listening on port " ++ show port)
    run port app
    where
        port = 3000

mergeTransactions :: [M.Transaction] -> [M.Transaction] -> [M.Transaction]
mergeTransactions new old = old ++ deleteFirstsBy eqOnAllButTags new old
    where
        eqOnAllButTags = and2 [(==) `on` M.date, (==) `on` M.description, (==) `on` M.amount]
        and2 :: [a -> a -> Bool] -> a -> a -> Bool
        and2 fs a b = all (\f -> f a b) fs

updateThread :: IO ()
updateThread = forever $ do
    new <- getLatestTransactions
    void $ MDB.updateTransactions (mergeTransactions new)
    now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> getCurrentTime
    putStrLn $ "updated transactions " ++ now
    threadDelay $ periodInMinutes * 60 * (10 ^ (6 :: Int))
        where
            periodInMinutes = 60

app :: Application
app req = route (pathInfo req) req

--route :: [Text] -> Request -> IO Response
route :: (IsString a, Eq a) => [a] -> Application
route path
    | null path = notFound -- TODO ghcjs output
    | path == ["transactions"] = transactions
    | otherwise = notFound

transactions :: Application
transactions _ respond = do
    ts <- fromMaybe [] <$> MDB.getTransactions
    respond $ responseLBS
        ok200
        [("Content-Type", "application/json")]
        (fromString $ JSON.encode ts)

notFound :: Application
notFound req continuation = continuation $ responseLBS ok200 [] $ "not found" <> fromString (show $ pathInfo req)
