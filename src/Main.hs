{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.Trans.Either (bimapEitherT, eitherT)
import           Data.Function              (on, (&))
import           Data.List                  (deleteFirstsBy)
import           Data.String                (fromString)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           DB                         (DB, DBConfig, Transactions, runDB)
import qualified DB                         (Error, connect, getTransactions,
                                             lift, updateTransactions)
import           Money                      (Transaction (..))
import           Network.HTTP.Types.Status  (ok200)
import           Network.Wai                (Application, responseFile)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Scrapers                   (Credential)
import           Scrapers.BankOfAmerica     (getLatestTransactions)
import           Servant                    ((:<|>) (..), (:>), (:~>) (..), Get,
                                             JSON, Proxy (..), Put, Raw,
                                             ReqBody, ServantErr (..), Server,
                                             ServerT, enter, err500, serve)

type API = MoneyAPI :<|> StaticAPI

type MoneyAPI =
         "transactions" :> Get '[JSON] Transactions
    :<|> "credentials"  :> ReqBody '[JSON] Credential
                        :> Put '[JSON] Bool

type StaticAPI = "" :> Raw

api :: Proxy API
api = Proxy

app :: DBConfig -> Application
app ctx = serve api (server ctx)

server :: DBConfig -> Server API
server ctx = enter (Nat (bimapEitherT dbErrorToServantErr id . runDB ctx)) dbServer :<|> staticServer

dbErrorToServantErr :: DB.Error -> ServantErr
dbErrorToServantErr = (\x -> err500 { errBody = x }) . fromString . show

staticServer :: Server StaticAPI
staticServer _ respond = respond $ responseFile ok200 [("Content-Type", "text/html")] "index.html" Nothing

dbServer :: ServerT MoneyAPI DB
dbServer = transactions
      :<|> credentials

transactions :: DB Transactions
transactions = fst <$> DB.getTransactions

credentials :: Credential -> DB Bool
credentials = error "credentials endpoint not implemented"

main :: IO ()
main = do
    dbConfig <- DB.connect
    void $ forkIO $ eitherT print return $ runDB dbConfig updateThread
    putStrLn "listening on port 3000"
    runSettings (defaultSettings & setPort 3000 & setHost "127.0.0.1")
                (app dbConfig)

mergeTransactions :: [Transaction] -> [Transaction] -> [Transaction]
mergeTransactions new old = old ++ deleteFirstsBy eqOnAllButTags new old
    where
        eqOnAllButTags = and2 [(==) `on` date, (==) `on` description, (==) `on` amount]
        and2 :: [a -> a -> Bool] -> a -> a -> Bool
        and2 fs a b = all (\f -> f a b) fs

updateThread :: DB ()
updateThread = forever $ do
    new <- DB.lift getLatestTransactions
    void $ DB.updateTransactions (mergeTransactions new)
    now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> DB.lift getCurrentTime
    DB.lift $ putStrLn $ "updated transactions " ++ now
    DB.lift $ threadDelay $ periodInMinutes * 60 * (10 ^ (6 :: Int))
        where
            periodInMinutes = 60
