{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Scrapers.BankOfAmerica (getLatestTransactions) where

import           Control.Lens           (view, (&))
import           Control.Monad          (void)
import Data.List (isInfixOf)
import qualified Data.ByteString        as BS (ByteString)
import           Data.ByteString.Lazy   (toStrict)
import           Data.ByteString.UTF8   (toString)
import           Data.Maybe             (listToMaybe, fromMaybe, mapMaybe)
import           Data.String            (fromString)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Money                  (Transaction)
import           Network.Wreq           (defaults, responseBody)
import           ParseCSV               (parseCredit, parseDebit)
import           Prelude                hiding (head, id, last, print, (!!))
import           Scrapers.Browser       (get, lift, post, runBrowserWithLog)
import           Scrapers.Common        (queryParamsFromUrl)
import           Text.HandsomeSoup.Util (css', cssSingle')
import           Text.XML.HXT.Core      (getAttrValue, getChildren, getText,
                                         hasName, (>>>))

getAnswer :: String -> BS.ByteString
getAnswer s = maybe (error "secret answer not found") snd $ listToMaybe $ filter (\(k, _) -> k `isInfixOf` s)
    [ ("pet"   , "***REMOVED***")
    , ("car"   , "***REMOVED***")
    , ("friend", "***REMOVED***")
    ]

username :: BS.ByteString
username = "***REMOVED***"

password :: BS.ByteString
password = "***REMOVED***"

homeUrl, baseUrl, loginUrl, challengeAnswerUrl :: String
homeUrl = "https://www.bankofamerica.com/"
baseUrl = "https://secure.bankofamerica.com/"
loginUrl = baseUrl ++ "login/sign-in/entry/signOnV2.go"
challengeAnswerUrl = baseUrl ++ "login/sign-in/validateChallengeAnswerV2.go"

checkingAccountDownloadUrl, creditCardStmtListUrl :: String -> String
checkingAccountDownloadUrl adx = baseUrl ++ "myaccounts/details/deposit/download-transactions.go?adx=" ++ adx
creditCardStmtListUrl adx = baseUrl ++ "myaccounts/brain/redirect.go?target=acctdetails&adx=" ++ adx ++ "&fsd=y&request_locale=en-us&source=add"

creditCardDownloadUrl :: String -> String -> String
creditCardDownloadUrl adx stx = baseUrl ++ "myaccounts/details/card/download-transactions.go?&adx=" ++ adx ++ "&stx=" ++ stx ++ "&target=downloadStmtFromDateList&formatType=csv"

csrfTokenSelector, secretQuestionSelector :: String
csrfTokenSelector = "#csrfTokenHidden"
secretQuestionSelector = "[for=tlpvt-challenge-answer]"

-- TODO exceptionhandling, and logging
getLatestTransactions :: IO [Transaction]
getLatestTransactions = fmap fst $
    runBrowserWithLog defaults $ do
        void $ get homeUrl
        loginResponse <- view responseBody <$>
            post loginUrl
                [ ("Access_ID", username)
                , ("passcode", password)
                ]

        let mCsrfToken = cssSingle' csrfTokenSelector (getAttrValue "value") loginResponse
        let mSecretQuestionText = cssSingle' secretQuestionSelector (getChildren >>> getText) loginResponse
        mChallengeAnswerResponse <- mCsrfToken & maybe (fail "no csrfToken") (\csrfToken ->
            mSecretQuestionText & maybe (return Nothing) (\secretQuestionText ->
                Just . view responseBody <$>
                    post challengeAnswerUrl
                        [ ("csrfTokenHidden", fromString csrfToken)
                        , ("challengeQuestionAnswer", getAnswer secretQuestionText)
                        , ("challengeQuestionAnswer-masked", getAnswer secretQuestionText)
                        ]))

        let accountView = mChallengeAnswerResponse & fromMaybe (error "implement me")
        let bankAccountAdxs = css' ".AccountItemDeposit" (getAttrValue "data-adx") accountView
        let creditCardAdxs = css' ".AccountItemCreditCard" (getAttrValue "data-adx") accountView

        bankAccountTransactions <- concat <$> mapM (\adx -> do
            now <- lift getCurrentTime
            let showTime = fromString . formatTime defaultTimeLocale "%m/%d/%Y"
            let oneYearInSeconds = 365*24*60*60
            (parseDebit . toString . toStrict . view responseBody) <$>
              post (checkingAccountDownloadUrl adx)
                [ ("downloadTransactionType", "customRange")
                , ("searchBean.timeFrameEndDate", showTime now)
                , ("searchBean.timeFrameStartDate", showTime $ addUTCTime (-oneYearInSeconds) now)
                , ("formatType", "csv")
                , ("searchBean.searchMoreOptionsPanelUsed", "false")
                ]) bankAccountAdxs

        creditCardTransactions <- concat . concat <$> mapM (\adx -> do
          stmtList <- view responseBody <$> get (creditCardStmtListUrl adx)
          let stxs = mapMaybe (lookup "stx" . queryParamsFromUrl) $
                      css' "#goto_select_trans_top" (getChildren >>> hasName "option" >>> getAttrValue "value") stmtList
          mapM (fmap (parseCredit . toString . toStrict . view responseBody) . get . creditCardDownloadUrl adx) stxs) creditCardAdxs

        return $ bankAccountTransactions ++ creditCardTransactions
