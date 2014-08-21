{-# LANGUAGE OverloadedStrings #-}
import Control.Arrow ((&&&))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack, writeFile)
import Data.Default.Class (def)
import Data.List (intercalate, isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client (CookieJar)
import Network.HTTP.Client.TLS (mkManagerSettings)
import Network.Wreq (Options, Response, postWith, getWith, proxy, manager, httpProxy, responseCookieJar, responseBody, cookies)
import qualified Network.Wreq as W (FormParam((:=)), defaults)
import Network.Wreq.Types (Postable)
import Text.XML.Light

type StateIO s a = StateT s IO a

(^=) :: BS.ByteString -> BS.ByteString -> W.FormParam
(^=) = (W.:=)

defaults :: Options
defaults = W.defaults & proxy .~ httpProxy "localhost" 8080 & manager .~ Left (mkManagerSettings (TLSSettingsSimple True False False) Nothing)

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

toQName :: String -> QName
toQName s = QName s Nothing Nothing

hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal k v e = findAttr (toQName k) e == Just v

hasAttrValBy :: String -> (String -> Bool) -> Element -> Bool
hasAttrValBy k p e = maybe False p $ findAttr (toQName k) e

stateMain :: StateIO CookieJar ()
stateMain = do
    _ <- httpget "https://www.bankofamerica.com/"
    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/entry/signOn.go" [ "Access_ID" ^= username ]
    _ <- httppost "https://secure.bankofamerica.com/login/getCAAFSO" [ "pmdata" ^= "" ]
    challengePage <- (^. responseBody) `fmap` httpget "https://secure.bankofamerica.com/login/sign-in/signOn.go"

    let question = BS.pack `fmap` getQuestion (LBS.unpack challengePage)
    let csrfToken = BS.pack $ getCsrfToken $ LBS.unpack challengePage
    let sendAnswer q = void $ httppost "https://secure.bankofamerica.com/login/sign-in/validateChallengeAnswer.go" [ "csrfTokenHidden" ^= csrfToken, "challengeQuestionAnswer" ^= getAnswer q, "rembme" ^= "on" ]

    maybe (return ()) sendAnswer question
    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/validatePassword.go" [ "csrfTokenHidden" ^= csrfToken, "password" ^= password ]
    debit <- httppost "https://secure.bankofamerica.com/myaccounts/details/deposit/download-transactions.go" [ "selectedTransPeriod" ^= ""
                                                                                                         , "downloadTransactionType" ^= "customRange"
                                                                                                         , "searchBean.timeFrameStartDate" ^= "03/01/2014"
                                                                                                         , "searchBean.timeFrameEndDate" ^= "08/17/2014" 
                                                                                                         , "formatType" ^= "csv" 
                                                                                                         , "searchBean.searchMoreOptionsPanelUsed" ^= "false" 
                                                                                                         ]
    creditStxs <- (^. responseBody) `fmap` httpget ("https://secure.bankofamerica.com/myaccounts/brain/redirect.go?source=overview&target=acctDetails&adx=" ++ adx)
    let stx = getStx $ LBS.unpack creditStxs
    credit <- httpget $ "https://secure.bankofamerica.com/myaccounts/details/card/download-transactions.go?&adx=" ++ adx ++ "&stx=" ++ stx ++ "&target=downloadStmtFromDateList&formatType=csv"
    lift $ LBS.writeFile "debit.csv" (debit ^. responseBody)
    lift $ LBS.writeFile "credit.csv" (credit ^. responseBody)
    where
        getQuestion :: String -> Maybe String
        getQuestion body = strContent `fmap` listToMaybe (mapMaybe (filterElement (hasAttrVal "for" "tlpvt-challenge-answer")) $ onlyElems $ parseXML body)
        getCsrfToken :: String -> String
        getCsrfToken body = fromMaybe (error "getCsrfToken") $ findAttr (toQName "value") =<< listToMaybe (mapMaybe (filterElement (hasAttrVal "name" "csrfTokenHidden")) $ onlyElems $ parseXML body)
        adx = undefined

fromMaybe :: a -> Maybe a -> a
fromMaybe d Nothing = d
fromMaybe _ (Just a) = a

safeHead :: a -> [a] -> a
safeHead d xs = head (xs ++ [d])

safeLast :: a -> [a] -> a
safeLast d [] = d
safeLast _ xs = last xs 

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl = map ((safeHead "" &&& safeLast "") . splitOn "=") . splitOn "&" . safeLast "" . splitOn "?"

getStx :: String -> String
getStx = fst . head . getStxs

getStxs :: String -> [(String, String)]
getStxs x = mapMaybe (\e -> do
    r <- findAttr (toQName "rel") e
    stx <- lookup "stx" $ queryParamsFromUrl r
    n <- findAttr (toQName "name") e
    let date = intercalate "/" $ reverse $ take 3 $ reverse $ splitOn "_" n
    return (stx, date)) $ concatMap (filterElements $ hasAttrValBy "name" ("goto_transactions_top_for_this_date" `isPrefixOf`)) $ onlyElems $ parseXML x

username :: BS.ByteString
username = ""

password :: BS.ByteString
password = ""

getAnswer :: BS.ByteString -> BS.ByteString
getAnswer = undefined
    
main :: IO ()
main = evalStateT stateMain def
