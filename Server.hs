{-# LANGUAGE OverloadedStrings #-}
module Main where

import BoaScrape (getLatestTransactions)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (void, forever)
import Data.List (deleteFirstsBy)
import Data.Maybe (fromMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified ManageDB as MDB (getTransactions, updateTransactions)
import qualified Money as M (Transaction(..))
import Network.Wai (Request, Response, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Util (responseHtml, responseString, stringToBase64, base64ToString)

main :: IO ()
main =  do
    _ <- forkIO updateThread
    putStrLn ("Listening on port " ++ show port)
    run port app
    where
        port = 3000

mergeTransactions :: [M.Transaction] -> [M.Transaction] -> [M.Transaction]
mergeTransactions new old = old ++ deleteFirstsBy (\a b -> M.date a == M.date b && M.description a == M.description b && M.amount a == M.amount b) new old

updateThread :: IO ()
updateThread = forever $ do
    new <- getLatestTransactions
    void $ MDB.updateTransactions (mergeTransactions new)
    threadDelay $ periodInSeconds * (10 ^ (6 :: Int))
        where
            periodInSeconds = 60

app :: Request -> IO Response
app req = route (pathInfo req) req

route :: [Text] -> Request -> IO Response
route path
    | null path = overviewApp
    | path == ["transaction"] = transactionApp
    | otherwise = notFound

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ show (pathInfo req)

overviewApp :: Request -> IO Response
overviewApp _ = do
    ts <- getTs
    return $ responseHtml $ renderHtml $ transactionsTable ts

transactionApp :: Request -> IO Response
transactionApp req = return $ responseString $ base64ToString $ show $ fromMaybe "" $ fromMaybe (Just "") $ lookup "v" $ queryString req

transactionsTable :: [M.Transaction] -> H.Html
transactionsTable ts = H.docTypeHtml $ do
    H.head $
        H.title "Overview Page"
    H.body $
        _table $ do
            H.tr $
                mapM_ td ["Date", "Description", "Amount", "Tags"]
            mapM_ (\t -> H.tr $ do
                td $ H.toHtml $ show $ M.date t
                td $ H.a H.! A.href (fromString $ "/transaction?v=" ++ stringToBase64 (show t)) $ H.toHtml $ M.description t
                td $ H.toHtml $ fromIntegral (M.amount t)/(100 :: Double)
                td $ H.toHtml $ show $ M.tags t) ts
    where
        _table = H.table H.! A.style "border-collapse: collapse; border: 1px solid black;"
        td = H.td H.! A.style "padding: .25em .25em .25em .25em; border-collapse: collapse; border: 1px solid black;"

-- TODO error reporting
getTs :: IO [M.Transaction]
getTs = fromMaybe [] <$> MDB.getTransactions
