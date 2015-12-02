{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Scrapers.BankOfAmerica (getAllTransactions) where

import           Control.Lens           (view, (&))
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString        as BS (ByteString)
import           Data.ByteString.Lazy   (toStrict)
import           Data.ByteString.UTF8   (toString)
import           Data.List              (isInfixOf)
import           Data.Maybe             (fromMaybe, listToMaybe, mapMaybe)
import           Data.String            (fromString)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Data.Time.Format       (defaultTimeLocale, formatTime)
import           Money                  (Transaction)
import           Network.Wreq           (defaults, responseBody)
import           ParseCSV               (parseCredit, parseDebit)
import           Prelude                hiding (head, id, last, print, (!!))
import           Scrapers.Browser       (get, post, runBrowserWithLog)
import           Scrapers.Common        (Cred (..), queryParamsFromUrl)
import           Text.HandsomeSoup.Util (css', cssSingle')
import           Text.XML.HXT.Core      (getAttrValue, getChildren, getText,
                                         hasName, (>>>))

getAnswer :: String -> [(String, String)] -> BS.ByteString
getAnswer s maap = fromString <$> maybe (error "secret answer not found") snd $ listToMaybe $ filter (\(k, _) -> k `isInfixOf` s) maap

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
getAllTransactions :: Cred -> IO [Transaction]
getAllTransactions cred = fmap fst $
    runBrowserWithLog defaults $ do
        void $ get homeUrl
        loginResponse <- view responseBody <$>
            post loginUrl
                [ ("Access_ID", fromString $ username cred)
                , ("passcode", fromString $ password cred)
                ]

        let mCsrfToken = cssSingle' csrfTokenSelector (getAttrValue "value") loginResponse
        let mSecretQuestionText = cssSingle' secretQuestionSelector (getChildren >>> getText) loginResponse
        mChallengeAnswerResponse <- mCsrfToken & maybe (fail "no csrfToken") (\csrfToken ->
            mSecretQuestionText & maybe (return Nothing) (\secretQuestionText ->
                Just . view responseBody <$>
                    post challengeAnswerUrl
                        [ ("csrfTokenHidden", fromString csrfToken)
                        , ("challengeQuestionAnswer", getAnswer secretQuestionText $ secretQuestionAnswers cred)
                        , ("challengeQuestionAnswer-masked", getAnswer secretQuestionText $ secretQuestionAnswers cred)
                        ]))

        let accountView = mChallengeAnswerResponse & fromMaybe (error "implement me")
        let bankAccountAdxs = css' ".AccountItemDeposit" (getAttrValue "data-adx") accountView
        let creditCardAdxs = css' ".AccountItemCreditCard" (getAttrValue "data-adx") accountView

        bankAccountTransactions <- concat <$> mapM (\adx -> do
            now <- liftIO getCurrentTime
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
