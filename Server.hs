{-# LANGUAGE OverloadedStrings #-}
module Main where

import BoaScrape (getLatestTransactions)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (join, void, forever)
import qualified Data.ByteString.Char8 as BS (unpack)
import qualified Data.ByteString.Lazy.Char8 as LBS (pack)
import Data.Function (on)
import Data.List (sortBy, deleteFirstsBy)
import Data.String (fromString)
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import qualified ManageDB as MDB (getTransactions, updateTransactions)
import qualified Money as M (JUTCTime(..), Transaction(..), similarTransactions)
import Network.HTTP.Types (ok200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, pathInfo, queryString, responseLBS)
import Network.Wai.Handler.Warp (run)
import System.Locale (defaultTimeLocale)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5.Attributes as A
import Util (maybeRead, responseString, stringToBase64, base64ToString)

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

app :: Request -> IO Response
app req = route (pathInfo req) req

route :: [Text] -> Request -> IO Response
route path
    | null path = overviewApp
    | path == ["transaction"] = transactionApp
    | otherwise = notFound

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ show (pathInfo req)

jsonApp :: (Read a) => (a -> IO Response) -> Request -> IO Response
jsonApp f req = maybe (return $ responseString "maybeRead v = Nothing") f (do
    v <- join $ lookup "v" $ queryString req
    maybeRead $ base64ToString $ BS.unpack v)

responseHtml :: H.Html -> Response
responseHtml = responseLBS ok200 [(hContentType, "text/html")] . LBS.pack . renderHtml

table :: [H.Html] -> [[H.Html]] -> H.Html
table header xss = _table $ mapM_ (H.tr . mapM_ td) (header:xss)
    where
        _table = H.table H.! A.style "border-collapse: collapse; border: 1px solid black;"
        td = H.td H.! A.style "padding: .25em .25em .25em .25em; border-collapse: collapse; border: 1px solid black;"

page :: H.Html -> H.Html -> H.Html
page title contents = H.docTypeHtml $ do
    H.head $
        H.title title
    H.body contents

transactionTableHeader :: [H.Html]
transactionTableHeader = ["Date", "Description", "Amount", "Tags"]

showDate :: M.JUTCTime -> String
showDate = formatTime defaultTimeLocale "%m/%d/%Y" . M.runJUTCTime

overviewApp :: Request -> IO Response
overviewApp _ = do
    ts <- MDB.getTransactions
    return $ maybe (responseString "MDB.getTransactions = Nothing") (responseHtml . pg) ts
    where
        pg :: [M.Transaction] -> H.Html
        pg ts = page "Overview" $ transactionsTable $ sortBy (flip compare `on` M.date) ts
        transactionsTable :: [M.Transaction] -> H.Html
        transactionsTable = table transactionTableHeader . map renderTransaction
        renderTransaction :: M.Transaction -> [H.Html]
        renderTransaction t = [ H.toHtml $ showDate $ M.date t
                              , H.a H.! A.href (fromString $ "/transaction?v=" ++ stringToBase64 (show t)) $ H.toHtml $ M.description t
                              , H.toHtml $ fromIntegral (M.amount t)/(100 :: Double)
                              , H.toHtml $ show $ M.tags t
                              ]

transactionApp :: Request -> IO Response
transactionApp = jsonApp f
    where
        f :: M.Transaction -> IO Response
        f t = do
            ts <- MDB.getTransactions
            return $ maybe (responseString "MDB.getTransactions = Nothing") (responseHtml . pg t) ts
        pg :: M.Transaction -> [M.Transaction] -> H.Html
        pg t ts = page "Transaction" $ table transactionTableHeader $ map renderTransaction (t:map snd (M.similarTransactions t ts))
        renderTransaction :: M.Transaction -> [H.Html]
        renderTransaction t = [ H.toHtml $ showDate $ M.date t
                              , H.a H.! A.href (fromString $ "/transaction?v=" ++ stringToBase64 (show t)) $ H.toHtml $ M.description t
                              , H.toHtml $ fromIntegral (M.amount t)/(100 :: Double)
                              , H.toHtml $ show $ M.tags t
                              ]
