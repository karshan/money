{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoMonomorphismRestriction  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# OPTIONS_GHC -fno-warn-unused-binds  #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module DB
    ( DB
    , DBContext
    , addCredential
    , runDB
    , openDB
    , getTransactions
    , addTags
    , removeTags
    , getCredentials
    , mergeTransactions
    , getCookieJar
    , putCookieJar
    , getLogs
    , addLog
    )
    where

import           Control.Lens           (view, (%~), (&), (.~), (^.))
import           Control.Lens.TH        (makeLenses)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (get, put)
import           Crypto.Hash            (Digest, SHA256, hash)
import           Data.Acid              (AcidState, Query, Update, makeAcidic,
                                         openLocalStateFrom, query, update)
import           Data.Aeson             (encode)
import           Data.ByteString        (ByteString)
import           Data.ByteString.Lazy   (toStrict)
import           Data.Char              (toLower)
import           Data.Default.Class     (def)
import           Data.Function          (on)
import           Data.List              (deleteFirstsBy, isInfixOf, nub)
import           Data.Maybe             (isJust)
import           Data.SafeCopy          (base, deriveSafeCopy)
import           Money                  (Transaction (..))
import           Network.HTTP.Client    (Cookie, CookieJar)
import           Scrapers.Browser       (LogRecord, RequestLog, ResponseLog)
import           Scrapers.Common        (Cred (..), Credential (..))

type DBContext = AcidState Database
data Database = Database {
    _transactions :: [Transaction]
  , _credentials  :: [Credential]
  , _cookieJar    :: CookieJar
  , _logs         :: [([LogRecord], Maybe String)]
  }

makeLenses ''Database

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''Transaction)
$(deriveSafeCopy 0 'base ''Credential)
$(deriveSafeCopy 0 'base ''Cred)
$(deriveSafeCopy 0 'base ''CookieJar)
$(deriveSafeCopy 0 'base ''Cookie)
$(deriveSafeCopy 0 'base ''ResponseLog)
$(deriveSafeCopy 0 'base ''RequestLog)

-- FIXME: the has shouldn't depend on the ordering of ts
--        we can hash transactions separately and then xor the hashes together
hashTransactions :: [Transaction] -> String
hashTransactions ts = show $ sha256 $ toStrict $ encode ts
    where
        sha256 :: ByteString -> Digest SHA256
        sha256 = hash

ciIsInfixOf :: String -> String -> Bool
ciIsInfixOf = isInfixOf `on` map toLower

addTags_ :: String -> String -> String -> Update Database Bool
addTags_ rev filter' tag = do
    db <- get
    if rev /= hashTransactions (db ^. transactions) then
        return False
    else do
        put $ db & transactions %~
                    map (\t -> if filter' `ciIsInfixOf` description t
                             then t { tags = nub $ tag:tags t }
                             else t)
        return True

removeTags_ :: String -> String -> String -> Update Database Bool
removeTags_ rev filter' tag = do
    db <- get
    if rev /= hashTransactions (db ^. transactions) then
        return False
    else do
        put $ db & transactions %~
                    map (\t -> if filter' `ciIsInfixOf` description t
                             then t { tags = filter (/= tag) $ tags t }
                             else t)
        return True

-- returns the number of new transactions (those that are not already in the db)
mergeTransactions_ :: [Transaction] -> Update Database Int
mergeTransactions_ new = do
    db <- get
    let (numUpdated, updated) = mergeTransactionsPure new $ db ^. transactions
    put (db & transactions .~ updated)
    return numUpdated

mergeTransactionsPure :: [Transaction] -> [Transaction] -> (Int, [Transaction])
mergeTransactionsPure new old = (length output - length old, output)
   where
       eqOnAllButTags = and2 [(==) `on` date, (==) `on` description, (==) `on` amount]
       and2 :: [a -> a -> Bool] -> a -> a -> Bool
       and2 fs a b = all (\f -> f a b) fs
       output :: [Transaction]
       output = old ++ deleteFirstsBy eqOnAllButTags new old

getTransactions_ :: Query Database [Transaction]
getTransactions_ = view transactions <$> ask

addCredential_ :: Credential -> Update Database ()
addCredential_ cred = put . (credentials %~ (cred:)) =<< get

getCredentials_ :: Query Database [Credential]
getCredentials_ = view credentials <$> ask

getCookieJar_ :: Query Database CookieJar
getCookieJar_ = view cookieJar <$> ask

putCookieJar_ :: CookieJar -> Update Database ()
putCookieJar_ cj = put . (cookieJar .~ cj) =<< get

getLogs_ :: Query Database [([LogRecord], Maybe String)]
getLogs_ = view logs <$> ask

addLog_ :: ([LogRecord], Maybe String) -> Update Database ()
addLog_ l = put . (logs %~ (l:)) =<< get

$(makeAcidic ''Database [ 'mergeTransactions_, 'getTransactions_
                        , 'addCredential_, 'getCredentials_
                        , 'getCookieJar_, 'putCookieJar_
                        , 'getLogs_, 'addLog_
                        , 'addTags_, 'removeTags_
                        ])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database [] [] def [])


update' a = liftIO . update a
query' a = liftIO . query a

--TODO generate with TemplateHaskell
getTransactions :: DB (String, [Transaction])
getTransactions = (\ts -> (hashTransactions ts, ts)) <$> ((`query'` GetTransactions_) =<< ask)

mergeTransactions :: [Transaction] -> DB Int
mergeTransactions new = (`update'` MergeTransactions_ new) =<< ask

addCredential :: Credential -> DB ()
addCredential cred = (`update'` AddCredential_ cred) =<< ask

getCredentials :: DB [Credential]
getCredentials = (`query'` GetCredentials_) =<< ask

getCookieJar :: DB CookieJar
getCookieJar = (`query'` GetCookieJar_) =<< ask

putCookieJar :: CookieJar -> DB ()
putCookieJar cj = (`update'` PutCookieJar_ cj) =<< ask

getLogs :: DB [([LogRecord], Maybe String)]
getLogs = filter (isJust . snd) <$> ((`query'` GetLogs_) =<< ask)

addLog :: ([LogRecord], Maybe String) -> DB ()
addLog l = (`update'` AddLog_ l) =<< ask

addTags :: (String, String, String) -> DB Bool
addTags (rev, filter', tag) = (`update'` AddTags_ rev filter' tag) =<< ask

removeTags :: (String, String, String) -> DB Bool
removeTags (rev, filter', tag) = (`update'` RemoveTags_ rev filter' tag) =<< ask
