{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ConstraintKinds     #-}
module Main where

import           Control.Concurrent         (forkIO, threadDelay)
import           Control.Exception          (SomeException, try)
import           Control.Lens               ((^.), (^?))
import           Control.Monad              (join, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (ExceptT)
import           Crypto.Cipher.AES.Util     (cbcDecrypt', cbcEncrypt')
import           Crypto.Random              (getRandomBytes)
import           Data.Aeson.Lens            (key, _String)
import           Data.Aeson                 (decode)
import           Data.Bool                  (bool)
import           Data.ByteString            (ByteString)
import qualified Data.ByteString            as BS (map)
import qualified Data.ByteString.Base64.URL as URL
import qualified Data.ByteString.Lazy       as L (readFile)
import           Data.ByteString.Lazy       (toStrict)
import           Data.ByteString.UTF8       (toString)
import           Data.Char                  (isDigit, isPrint, isSpace, ord)
import           Data.Function              ((&))
import           Data.List.Split            (splitOn)
import           Data.Monoid                ((<>))
import           Data.String                (fromString)
import qualified Data.Text                  as T (unpack)
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Time.Format           (defaultTimeLocale, formatTime,
                                             parseTimeM)
import           DB                         (DBContext, openDB, DB',
                                             runDB)
import qualified DB                         (getTxnDb, mergeTxns)
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (badRequest400, found302)
import           Network.Wai                (Application, queryString,
                                             requestHeaders, pathInfo,
                                             responseLBS)
import           Network.Wai.Handler.Warp   (defaultSettings, runSettings,
                                             setHost, setPort)
import           Network.Wreq               (get, post, responseBody)
import           Servant                    ((:<|>) (..), (:~>) (..),
                                             Proxy (..), 
                                             ServantErr (..), Server,
                                             ServerT, enter, serve)
import           Servant.Utils.StaticFiles  (serveDirectory)
import           Money.API                  (MoneyAPI, API)
import           Data.SafeCopy              (safePut)
import           Data.Serialize             (runPut)
import           Plaid                      (getTxns, PlaidCredentials)
import           Data.IxSet.Typed           (toSet)

serverBaseUrl :: ByteString
serverBaseUrl = "https://money.karshan.me/"

api :: Proxy API
api = Proxy

app :: DBContext -> Application
app ctx = serve api (server ctx)

server :: DBContext -> Server API
server ctx = enter (Nat nat) dbServer :<|> serveDirectory "frontend/servedir/"
    where
        nat :: DB' a -> ExceptT ServantErr IO a
        nat a = liftIO $ runDB ctx a

dbServer :: ServerT MoneyAPI DB'
dbServer = toSet <$> DB.getTxnDb

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
authMiddleware passphrase (googClientId, googClientSecret) googIds mainApp req respond =
    either (\(s :: SomeException) -> error $ "auth exception " <> show s) id <$> try (do
        let mCookie = lookup cookieName . parseCookies =<< lookup hCookie (requestHeaders req)
        cond <- maybe (return False) validateCookie mCookie
        if cond then
            if null (pathInfo req) then
                respond $ responseLBS found302 [("Location", serverBaseUrl <> "static/ghcjs-out/index.html")] ""
            else
                mainApp req respond
        else
            join (lookup "code" (queryString req)) & maybe authRedirect
                 (bool invalidCode setCookieRedirect <=< validateGoogleCode))
    where
        authRedirect = respond $ responseLBS found302 [("Location", authRedirectUrl)] ""

        invalidCode = respond $ responseLBS badRequest400 [] "invalid code"

        authRedirectUrl = "https://accounts.google.com/o/oauth2/v2/auth?response_type=code&client_id=" <> googClientId <> "&redirect_uri=" <> serverBaseUrl <> "&scope=profile"

        setCookieRedirect = generateCookie >>= (\cookie -> respond $ responseLBS found302 [("Location", authRedirectUrl), ("Set-Cookie", fromString cookieName <> "=" <> cookie <> "; Path=/; Secure; HttpOnly;")] "")

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
              print access_token
              body <- (^. responseBody) <$> get ("https://www.googleapis.com/plus/v1/people/me?access_token=" <> T.unpack access_token)
              return $ maybe False ((`elem` googIds) . T.unpack) $ toString (toStrict body) ^? key "id" . _String)
        cookieName = "money-session"

main :: IO ()
main = do
    db <- openDB "transactions.aciddb"
    (googClientId, googClientSecret) <- (\[a,b] -> (a,b)) . map (fromString . filter isPrint) . lines <$> readFile "goog-creds"
    googIds <- map (filter isDigit) . lines <$> readFile "goog-ids"
    Just plaidCreds <- decode <$> L.readFile "plaid-creds.json"
    forkIO (updateThread plaidCreds db)
    passphrase <- getRandomBytes 32
    putStrLn "listening on port 3000"
    runSettings (defaultSettings & setPort 3000 & setHost "127.0.0.1")
                (authMiddleware passphrase (googClientId, googClientSecret) googIds $ app db)

updateThread :: PlaidCredentials -> DBContext -> IO ()
updateThread plaidCreds db = do
    eNewTxns <- getTxns plaidCreds
    either
        print
        (\(newTxns, bals) -> do
            numNewTxns <- runDB db
                (DB.mergeTxns (newTxns, bals))
            putStrLn $ (show numNewTxns) ++ " new transactions")
        eNewTxns
    threadDelay (6 * 3600 * 10^(6 :: Int))
    updateThread plaidCreds db
