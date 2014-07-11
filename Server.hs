{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified ManageDB as MDB (getTransactions)
import qualified Money as M (Transaction(..))
import Network.Wai (Request, Response, pathInfo)
import Network.Wai.Handler.Warp (run)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Util (responseHtml, responseString, table)

main :: IO ()
main = putStrLn ("Listening on port " ++ show port) >> run port app
    where
        port = 3000

app :: Request -> IO Response
app req = route (pathInfo req) req

route :: [Text] -> Request -> IO Response
route path
    | null path = overviewApp
    | otherwise = notFound

notFound :: Request -> IO Response
notFound req = return $ responseString $ "not found " ++ show (pathInfo req)

overviewApp :: Request -> IO Response
overviewApp _ = do
    ts <- getTs
    return $ responseHtml $ renderHtml $ transactionsTable ts

transactionsTable :: [M.Transaction] -> H.Html
transactionsTable ts = H.docTypeHtml $ do
    H.head $
        H.title "Overview Page"
    H.body $
        table $ ["Date", "Description", "Amount", "Tags"]
                : map (\t -> map ($ t) [show . M.date, M.description, show . M.amount, show . M.tags]) ts

-- TODO error reporting
getTs :: IO [M.Transaction]
getTs = fromMaybe [] <$> MDB.getTransactions
