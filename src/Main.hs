{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Lens               ((^.), (^?))
import           Control.Monad              (forever, join, void, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT)
import           Data.Aeson.Lens            (key, _String)
import           Data.Bool                  (bool)
import           Data.ByteString            (ByteString)
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.UTF8       (toString)
import           Data.Char                  (isSpace)
import           Data.Function              ((&))
import           Data.List.Split            (splitOn)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
import qualified Data.Text                  as T (unpack)
import           Data.Time.Clock            (getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime)
import           DB                         (DB, DBContext, openDB, runDB)
import qualified DB                         (addCredential, addTags,
                                             getCredentials, getLogs,
                                             getTransactions, mergeTransactions,
                                             removeTags)
import           Money                      (Transaction (..))
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (found302, notFound404, ok200)
import           Network.Wai                (Application, ResponseReceived,
                                             pathInfo, queryString,
                                             requestHeaders, responseFile,
                                             responseLBS)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Network.Wreq               (defaults, get, post, responseBody)
import qualified Scrapers                   (getAllTransactions)
import           Scrapers.Browser           (LogRecord)
import           Scrapers.Common            (Credential, showCredential)
import           Servant                    ((:<|>) (..), (:>), (:~>) (..), Get,
                                             JSON, Proxy (..), Put, Raw,
                                             ReqBody, ServantErr (..), Server,
                                             ServerT, enter, serve)

serverBaseUrl = "https://karshan.me"

type API = MoneyAPI :<|> StaticAPI

type MoneyAPI =
         "transactions"  :> Get '[JSON] (String, [Transaction])
    :<|> "addTags"       :> ReqBody '[JSON] (String, String, String)
                         :> Put '[JSON] Bool
    :<|> "removeTags"    :> ReqBody '[JSON] (String, String, String)
                         :> Put '[JSON] Bool
    :<|> "credentials"   :> Get '[JSON] [(String, String)] -- [(service, username)]
    :<|> "addCredential" :> ReqBody '[JSON] Credential
                         :> Put '[JSON] ()
    :<|> "logs"          :> Get '[JSON] [([LogRecord], Maybe String)]

type StaticAPI = Raw

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
staticServer request respond = if null (pathInfo request) then respond $ responseFile ok200 [("Content-Type", "text/html")] "elm-frontend/index.html" Nothing else respond $ responseLBS notFound404 [] "not found"

dbServer :: ServerT MoneyAPI DB
dbServer = DB.getTransactions
      :<|> DB.addTags
      :<|> DB.removeTags
      :<|> (map showCredential <$> DB.getCredentials)
      :<|> DB.addCredential
      :<|> DB.getLogs

trim :: String -> String
trim = takeWhile (not . isSpace) . dropWhile isSpace

parseCookies :: ByteString -> [(String, String)]
parseCookies = map (\[a,b] -> (a,b)) . filter ((== 2) . length) . map (splitOn "=" . trim) . splitOn ";" . toString

googClientId = "235611429010-mnet4dc9cpkcafufqogdtrv86t54u9hi.apps.googleusercontent.com"
googClientSecret = "YxUKnQO2Nb4qEB0c3VHlbzXR"

--TODO exception handling
authMiddleware :: Application -> Application
authMiddleware mainApp req respond =
    if maybe False validateCookie (lookup cookieName . parseCookies =<< lookup hCookie (requestHeaders req)) then
        mainApp req respond
    else
        join (lookup "code" (queryString req)) & maybe authRedirect
            (bool authRedirect setCookieRedirect <=< validateGoogleCode)
    where
      authRedirect :: IO ResponseReceived
      authRedirect = respond $ responseLBS found302 [("Location", authRedirectUrl)] ""
      authRedirectUrl = "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=" <> googClientId <> "&redirect_uri=" <> serverBaseUrl <> "&scope=profile"
      setCookieRedirect :: IO ResponseReceived
      setCookieRedirect = generateCookie >>= (\cookie -> respond $ responseLBS found302 [("Location", authRedirectUrl), ("Set-Cookie", fromString cookieName <> "=" <> cookie)] "")
      generateCookie :: IO ByteString
      generateCookie = return "lolwut123"
      validateCookie :: String -> Bool
      validateCookie = (== "lolwut123")
      validateGoogleCode :: ByteString -> IO Bool
      validateGoogleCode code = do
          mToken <-  ((^? _String) <=< (^? key "access_token")) . toString . toStrict . (^. responseBody) <$> post "https://www.googleapis.com/oauth2/v4/token"
                    [ ("code" :: ByteString, code)
                    , ("client_id", googClientId)
                    , ("client_secret", googClientSecret)
                    , ("redirect_uri", serverBaseUrl)
                    , ("grant_type", "authorization_code")
                    ]
          mToken & maybe (return False) (\access_token -> do
              body <- (^. responseBody) <$> get ("https://www.googleapis.com/plus/v1/people/me?access_token=" <> T.unpack access_token)
              print body
              return $ (toString (toStrict body) ^? key "id") == Just "109960628294760037742")
      cookieName = "money-session"

main :: IO ()
main = do
    db <- openDB "transactions.aciddb"
    void $ forkIO $ runDB db updateThread
    putStrLn "listening on port 3000"
    runSettings (defaultSettings & setPort 3000 & setHost "127.0.0.1")
                (authMiddleware $ app db)

updateThread :: DB ()
updateThread = forever $ do
    let periodUs = 60 * 60 * 10 ^ (6 :: Int) -- 1 minute = 60 * 10 ^ 6
    result <- Scrapers.getAllTransactions
    result & either (\e -> liftIO (print e) >> liftIO (threadDelay periodUs)) (\new -> do
        numNewTs <- DB.mergeTransactions new
        now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> liftIO getCurrentTime
        liftIO $ putStrLn $ "updated transactions " ++ now ++ ": " ++ show numNewTs ++ " new transactions"
        liftIO $ threadDelay periodUs)
