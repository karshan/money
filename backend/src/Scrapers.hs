{-# LANGUAGE LambdaCase #-}
module Scrapers
    ( getAllTransactions
    ) where

import           Control.Monad          (when)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe             (isJust)
import           DB                     (DB)
import qualified DB                     (addLog, getCookieJar, getCredentials,
                                         putCookieJar)
import           Money                  (Transaction)
import           Network.Wreq           (defaults)
import qualified Scrapers.BankOfAmerica as BankOfAmerica (getAllTransactions)
import           Scrapers.Browser       (Error, runBrowser)
import           Scrapers.Common        (Credential (..))

getAllTransactions :: DB (Either Error [Transaction])
getAllTransactions = do
    creds <- DB.getCredentials
    cj <- DB.getCookieJar
    (result, cookieJar, log_) <- liftIO $ runBrowser cj defaults $ concat <$> mapM (\case
        BankOfAmericaCreds c -> BankOfAmerica.getAllTransactions c
        _                    -> return []) creds
    let exception = either (Just . show) (const Nothing) result
    when (isJust exception) $ DB.addLog (log_, exception)
    DB.putCookieJar cookieJar
    return result
