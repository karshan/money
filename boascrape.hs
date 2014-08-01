{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import qualified Data.ByteString.Char8 as BS (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, putStr)
import Data.Default.Class (def)
import Data.Monoid ((<>))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options, FormParam((:=)), postWith, getWith, proxy, manager, httpProxy, responseCookieJar, responseBody, cookies)
import qualified Network.Wreq as W (defaults)
import Network.Wreq.Types (Postable)

type StateIO s a = StateT s IO a

(^=) :: BS.ByteString -> BS.ByteString -> FormParam
(^=) = (:=)

defaults :: Options
defaults = W.defaults & proxy .~ httpProxy "localhost" 8080 & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

httppost :: Postable a => String -> a -> StateIO CookieJar ()
httppost url params = do
    icj <- get
    ocj <- lift (
        let opts = defaults & cookies .~ icj in do
        r <- postWith opts url params
        return $ r ^. responseCookieJar)
    put (icj <> ocj)
    lift $ print (url, icj <> ocj)

httpget :: String -> StateIO CookieJar LBS.ByteString
httpget url = do
    icj <- get
    (ocj, body) <- lift (
        let opts = defaults & cookies .~ icj in do
        r <- getWith opts url
        return (r ^. responseCookieJar, r ^. responseBody))
    put (icj <> ocj)
    lift $ print (url, icj <> ocj)
    return body

stateMain :: StateIO CookieJar LBS.ByteString
stateMain = do
    _ <- httpget "https://www.bankofamerica.com/"
    httppost "https://secure.bankofamerica.com/login/sign-in/entry/signOn.go" [ "Access_ID" ^= "username" ]
    httppost "https://secure.bankofamerica.com/login/getCAAFSO" [ "pmdata" ^= "" ]
    httpget "https://secure.bankofamerica.com/login/sign-in/signOn.go"
    

main :: IO ()
main = LBS.putStr =<< evalStateT stateMain def
