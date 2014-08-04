{-# LANGUAGE OverloadedStrings #-}
import Control.Lens ((&), (.~), (^.))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack, putStr)
import Data.Default.Class (def)
import Data.Monoid ((<>))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options, Response, postWith, getWith, proxy, manager, httpProxy, responseCookieJar, responseBody, cookies)
import qualified Network.Wreq as W (FormParam((:=)), defaults)
import Network.Wreq.Types (Postable)
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs (NTree)

type StateIO s a = StateT s IO a

(^=) :: BS.ByteString -> BS.ByteString -> W.FormParam
(^=) = (W.:=)

defaults :: Options
defaults = W.defaults & proxy .~ httpProxy "localhost" 8080 & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

css :: ArrowXml a => String -> a (NTree XNode) XmlTree
css tag = multi (hasName tag)

withCookies :: (Options -> IO (Response a)) -> StateIO CookieJar (Response a)
withCookies f = do
    cj <- get
    resp <- lift (f (defaults & cookies .~ cj))
    put (cj <> resp ^. responseCookieJar)
    return resp

httppost :: Postable a => String -> a -> StateIO CookieJar (Response LBS.ByteString)
httppost url params = withCookies (\o -> postWith o url params)

httpget :: String -> StateIO CookieJar (Response LBS.ByteString)
httpget url = withCookies (`getWith` url)

stateMain :: StateIO CookieJar ()
stateMain = do
    _ <- httpget "https://www.bankofamerica.com/"
    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/entry/signOn.go" [ "Access_ID" ^= username ]
    _ <- httppost "https://secure.bankofamerica.com/login/getCAAFSO" [ "pmdata" ^= "" ]
    challengePage <- (^. responseBody) `fmap` httpget "https://secure.bankofamerica.com/login/sign-in/signOn.go"

    let question = BS.pack $ getQuestion $ LBS.unpack challengePage
    let csrfToken = BS.pack $ getCsrfToken $ LBS.unpack challengePage

    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/validateChallengeAnswer.go" [ "csrfTokenHidden" ^= csrfToken, "challengeQuestionAnswer" ^= getAnswer question, "rembme" ^= "on" ]
    out <- httppost "https://secure.bankofamerica.com/login/sign-in/validatePassword.go" [ "csrfTokenHidden" ^= csrfToken, "password" ^= password ]
    (lift . print) =<< get
    lift $ LBS.putStr (out ^. responseBody)
    where
        getQuestion :: String -> String
        getQuestion body = head (runLA (hread >>> css "label" >>> hasAttrValue "for" (== "tlpvt-challenge-answer") //> getText) body ++ [""])
        getCsrfToken :: String -> String
        getCsrfToken body = head (runLA (hread >>> css "input" >>> hasAttrValue "name" (== "csrfTokenHidden") >>> getAttrValue "value") body ++ [""])

username :: BS.ByteString
username = undefined

password :: BS.ByteString
password = undefined

getAnswer :: BS.ByteString -> BS.ByteString
getAnswer = undefined
    
main :: IO ()
main = evalStateT stateMain def
