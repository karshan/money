{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Monad              (forever, void)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either
import           Data.Function              ((&))
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           DB                         (DB, DBContext, openDB, runDB)
import qualified DB                         (addCredential, getCredentials,
                                             getTransactions, mergeTransactions)
import           Money                      (Transaction (..))
import           Network.HTTP.Types.Status  (ok200)
import           Network.Wai                (Application, responseFile)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Scrapers                   (Credential, showCredential)
import qualified Scrapers                   (getAllTransactions)
import           Servant                    ((:<|>) (..), (:>), (:~>) (..), Get,
                                             JSON, Proxy (..), Put, Raw,
                                             ReqBody, ServantErr (..), Server,
                                             ServerT, enter, serve)

type API = MoneyAPI :<|> StaticAPI

type MoneyAPI =
         "transactions"  :> Get '[JSON] [Transaction]
    :<|> "credentials"   :> Get '[JSON] [(String, String)] -- [(service, username)]
    :<|> "addCredential" :> ReqBody '[JSON] Credential
                         :> Put '[JSON] ()

type StaticAPI = "" :> Raw

api :: Proxy API
api = Proxy

app :: DBContext -> Application
app ctx = serve api (server ctx)

server :: DBContext -> Server API
server ctx = enter (Nat nat) dbServer :<|> staticServer
    where
        nat :: DB a -> EitherT ServantErr IO a
        nat a = liftIO $ runDB ctx a

staticServer :: Server StaticAPI
staticServer _ respond = respond $ responseFile ok200 [("Content-Type", "text/html")] "index.html" Nothing

dbServer :: ServerT MoneyAPI DB
dbServer = DB.getTransactions
      :<|> (map showCredential <$> DB.getCredentials)
      :<|> DB.addCredential

main :: IO ()
main = do
    db <- openDB "transactions.aciddb"
    void $ forkIO $ runDB db updateThread
    putStrLn "listening on port 3000"
    runSettings (defaultSettings & setPort 3000 & setHost "127.0.0.1")
                (app db)

updateThread :: DB ()
updateThread = forever $ do
    creds <- DB.getCredentials
    new <- liftIO $ Scrapers.getAllTransactions creds
    numNewTs <- DB.mergeTransactions new
    now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> liftIO getCurrentTime
    liftIO $ putStrLn $ "updated transactions " ++ now ++ ": " ++ show numNewTs ++ " new transactions"
    liftIO $ threadDelay $ periodInMinutes * 60 * (10 ^ (6 :: Int))
        where
            periodInMinutes = 60
