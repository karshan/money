{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (SomeException, try)
import           Control.Lens               ((^.), (^?))
import           Control.Monad              (forever, join, void, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Either (EitherT)
import           Crypto.Cipher.AES.Util     (cbcDecrypt', cbcEncrypt')
import           Crypto.Random              (getRandomBytes)
import           Data.Aeson.Lens            (key, _String)
import           Data.Bool                  (bool)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS (map)
import qualified Data.ByteString.Base64.URL as URL
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.UTF8       (toString)
import           Data.Char                  (isDigit, isPrint, isSpace,
                                             ord)
import           Data.Function              ((&))
import           Data.List.Split            (splitOn)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
import qualified Data.Text                  as T (unpack)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           DB                         (DB, DBContext, openDB, runDB)
import qualified DB                         (addCredential, addTags,
                                             getCredentials, getLogs,
                                             getTransactions, mergeTransactions,
                                             removeTags)
import           Money                      (Transaction (..))
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (badRequest400, found302,
                                             notFound404, ok200)
import           Network.Wai                (Application, ResponseReceived,
                                             pathInfo, queryString,
                                             requestHeaders, responseFile,
                                             responseLBS)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Network.Wreq               (get, post, responseBody)
import qualified Scrapers                   (getAllTransactions)
import           Scrapers.Browser           (LogRecord)
import           Scrapers.Common            (Credential, showCredential)
import           Servant                    ((:<|>) (..), (:>), (:~>) (..), Get,
                                             JSON, Proxy (..), Put, Raw,
                                             ReqBody, ServantErr (..), Server,
                                             ServerT, enter, serve)

serverBaseUrl :: ByteString
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
parseCookies = map (\(a:b:_) -> (a,b)) . filter ((>= 2) . length) . map (splitOn "=" . trim) . splitOn ";" . toString

equalsToTilde, tildeToEquals :: ByteString -> ByteString
equalsToTilde = BS.map (\c -> if c == fromIntegral (ord '=') then fromIntegral $ ord '~' else c)
tildeToEquals = BS.map (\c -> if c == fromIntegral (ord '~') then fromIntegral $ ord '=' else c)

cookieEncode, cookieDecode :: ByteString -> ByteString
cookieEncode = equalsToTilde . URL.encode
cookieDecode = either (const "") id . URL.decode . tildeToEquals

--TODO exception handling
authMiddleware :: ByteString -> (ByteString, ByteString) -> [String] -> Application -> Application
authMiddleware passphrase (googClientId, googClientSecret) googIds mainApp req respond = either (\(s :: SomeException) -> error $ "auth exception " <> show s) id <$> try (do
    let mCookie = lookup cookieName . parseCookies =<< lookup hCookie (requestHeaders req)
    cond <- maybe (return False) validateCookie mCookie
    if cond then
         mainApp req respond
    else
        join (lookup "code" (queryString req)) & maybe authRedirect
             (bool invalidCode setCookieRedirect <=< validateGoogleCode))
    where
      authRedirect :: IO ResponseReceived
      authRedirect = respond $ responseLBS found302 [("Location", authRedirectUrl)] ""
      invalidCode :: IO ResponseReceived
      invalidCode = respond $ responseLBS badRequest400 [] "invalid code"
      authRedirectUrl = "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=" <> googClientId <> "&redirect_uri=" <> serverBaseUrl <> "&scope=profile"
      setCookieRedirect :: IO ResponseReceived
      setCookieRedirect = generateCookie >>= (\cookie -> respond $ responseLBS found302 [("Location", authRedirectUrl), ("Set-Cookie", fromString cookieName <> "=" <> cookie)] "")
      generateCookie :: IO ByteString
      generateCookie = either (error "fatal: cbcEncrypt' failed with: ") cookieEncode <$>
          (cbcEncrypt' passphrase =<< fromString . formatTime defaultTimeLocale "%s" <$> getCurrentTime)
      validateCookie :: String -> IO Bool
      validateCookie c = do
          now <- getCurrentTime
          maybe (return False) (\created -> return $ diffUTCTime now created < oneWeek) (parseTimeM True defaultTimeLocale "%s" . toString =<< eToM (cbcDecrypt' passphrase $ cookieDecode $ fromString c))
          where
              oneWeek = 7 * 24 * 60 * 60
              eToM = either (const Nothing) Just
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
              return $ maybe False ((`elem` googIds) . T.unpack) $ toString (toStrict body) ^? key "id" . _String)
      cookieName = "money-session"

main :: IO ()
main = do
    db <- openDB "transactions.aciddb"
    (googClientId, googClientSecret) <- (\[a,b] -> (a,b)) . map (fromString . filter isPrint) . lines <$> readFile "goog-creds"
    googIds <- map (filter isDigit) . lines <$> readFile "goog-ids"
    void $ forkIO $ runDB db updateThread
    passphrase <- getRandomBytes 32
    putStrLn "listening on port 3000"
    runSettings (defaultSettings & setPort 3000 & setHost "127.0.0.1")
                (authMiddleware passphrase (googClientId, googClientSecret) googIds $ app db)

updateThread :: DB ()
updateThread = forever $ do
    let periodUs = 60 * 60 * 10 ^ (6 :: Int) -- 1 minute = 60 * 10 ^ 6
    result <- Scrapers.getAllTransactions
    result & either (\e -> liftIO (print e) >> liftIO (threadDelay periodUs)) (\new -> do
        numNewTs <- DB.mergeTransactions new
        now <- formatTime defaultTimeLocale "%m/%d/%Y %T" <$> liftIO getCurrentTime
        liftIO $ putStrLn $ "updated transactions " ++ now ++ ": " ++ show numNewTs ++ " new transactions"
        liftIO $ threadDelay periodUs)
