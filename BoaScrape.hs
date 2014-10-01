{-# LANGUAGE OverloadedStrings #-}
module BoaScrape (getLatestTransactions) where

import Control.Applicative ((<$>))
import Control.Lens ((&), (.~), (^.))
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State (StateT, get, put, evalStateT)
import qualified Data.ByteString.Char8 as BS (ByteString, pack, isInfixOf)
import qualified Data.ByteString.Lazy.Char8 as LBS (ByteString, unpack)
import Data.Default.Class (def)
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Monoid ((<>))
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.Wreq (Options, Response, postWith, getWith, proxy, manager, httpProxy, responseCookieJar, responseBody, cookies)
import qualified Network.Wreq as W (FormParam((:=)), defaults)
import Network.Wreq.Types (Postable)
import System.Locale (defaultTimeLocale)
import Text.XML.Light (parseXML, onlyElems, QName(..), Element, strContent, findAttr, filterElement, filterElements)

import ParseCSV (parseDebit, parseCredit)
import Money (Transaction)

-- Utils
type StateIO s a = StateT s IO a

(^=) :: BS.ByteString -> BS.ByteString -> W.FormParam
(^=) = (W.:=)

defaults :: Options
defaults = W.defaults

--preserveCookies :: (Options -> IO (Response a)) -> StateIO CookieJar (Response a)
preserveCookies f = do
    cj <- get
    resp <- lift (f (defaults & cookies .~ cj))
    put (cj <> resp ^. responseCookieJar)
    return resp

--httppost :: Postable a => String -> a -> StateIO CookieJar (Response LBS.ByteString)
httppost url params = preserveCookies (\o -> postWith o url params)

--httpget :: String -> StateIO CookieJar (Response LBS.ByteString)
httpget url = preserveCookies (`getWith` url)

toQName :: String -> QName
toQName s = QName s Nothing Nothing

hasAttrVal :: String -> String -> Element -> Bool
hasAttrVal k v e = findAttr (toQName k) e == Just v

hasAttrValBy :: String -> (String -> Bool) -> Element -> Bool
hasAttrValBy k p e = maybe False p $ findAttr (toQName k) e

safeHead :: [a] -> Maybe a
safeHead = listToMaybe

queryParamsFromUrl :: String -> [(String, String)]
queryParamsFromUrl s = fromMaybe [] (do
    paramString <- safeHead $ reverse $ splitOn "?" s
    return $ mapMaybe ((\pair -> if length pair == 2 then Just (head pair, last pair) else Nothing) . splitOn "=") $ splitOn "&" paramString)

--withCookies :: StateIO CookieJar a -> IO a
withCookies s = evalStateT s def
-- End Utils

-- Config
adx :: String
adx = undefined

getAnswer :: BS.ByteString -> BS.ByteString
getAnswer = undefined

username :: BS.ByteString
username = undefined

password :: BS.ByteString
password = undefined

debitStartDate :: BS.ByteString
debitStartDate = undefined
-- End Config

getLatestTransactions :: IO [Transaction]
getLatestTransactions = withCookies $ do
    _ <- httpget "https://www.bankofamerica.com/"
    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/entry/signOn.go" [ "Access_ID" ^= username ]
    _ <- httppost "https://secure.bankofamerica.com/login/getCAAFSO" [ "pmdata" ^= "" ]
    challengePage <- (^. responseBody) <$> httpget "https://secure.bankofamerica.com/login/sign-in/signOn.go"

    let question = BS.pack <$> getQuestion (LBS.unpack challengePage)
    let csrfToken = BS.pack $ getCsrfToken $ LBS.unpack challengePage
    let sendAnswer q = void $ httppost "https://secure.bankofamerica.com/login/sign-in/validateChallengeAnswer.go" [ "csrfTokenHidden" ^= csrfToken, "challengeQuestionAnswer" ^= getAnswer q, "rembme" ^= "on" ]

    maybe (return ()) sendAnswer question
    _ <- httppost "https://secure.bankofamerica.com/login/sign-in/validatePassword.go" [ "csrfTokenHidden" ^= csrfToken, "password" ^= password ]
    now <- (BS.pack . formatTime defaultTimeLocale "%m/%d/%Y") <$> lift getCurrentTime
    debitcsv <- (^. responseBody) <$> httppost "https://secure.bankofamerica.com/myaccounts/details/deposit/download-transactions.go" [ "selectedTransPeriod" ^= ""
                                                                                                         , "downloadTransactionType" ^= "customRange"
                                                                                                         , "searchBean.timeFrameStartDate" ^= debitStartDate
                                                                                                         , "searchBean.timeFrameEndDate" ^= now
                                                                                                         , "formatType" ^= "csv" 
                                                                                                         , "searchBean.searchMoreOptionsPanelUsed" ^= "false" 
                                                                                                         ]
    creditStxs <- (^. responseBody) <$> httpget ("https://secure.bankofamerica.com/myaccounts/brain/redirect.go?source=overview&target=acctDetails&adx=" ++ adx)
    let stxs = getStxs $ LBS.unpack creditStxs
    creditTs <- concat <$> mapM (\stx -> (parseCredit . LBS.unpack . (^. responseBody)) <$>
        httpget ("https://secure.bankofamerica.com/myaccounts/details/card/download-transactions.go?&adx=" ++ adx ++ "&stx=" ++ stx ++ "&target=downloadStmtFromDateList&formatType=csv")) stxs
    return $ parseDebit (LBS.unpack debitcsv) ++ creditTs
    where
        getQuestion :: String -> Maybe String
        getQuestion body = strContent <$> safeHead (mapMaybe (filterElement (hasAttrVal "for" "tlpvt-challenge-answer")) $ onlyElems $ parseXML body)
        getCsrfToken :: String -> String
        getCsrfToken body = fromMaybe (error "getCsrfToken") $ findAttr (toQName "value") =<< safeHead (mapMaybe (filterElement (hasAttrVal "name" "csrfTokenHidden")) $ onlyElems $ parseXML body)
        getStxs :: String -> [String]
        getStxs x = fromMaybe [] (do
            divList <-safeHead $ mapMaybe (filterElement $ hasAttrVal "class" "goto-trans-dropdown-box hide") $ onlyElems $ parseXML x
            return $ mapMaybe (\e -> do
                r <- findAttr (toQName "rel") e
                lookup "stx" $ queryParamsFromUrl r) $ filterElements (hasAttrValBy "name" ("goto_" `isPrefixOf`)) divList)
