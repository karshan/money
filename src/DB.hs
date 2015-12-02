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
module DB
    ( DB
    , DBContext
    , addCredential
    , runDB
    , openDB
    , getTransactions
    , getCredentials
    , mergeTransactions
    )
    where

import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask, runReaderT)
import           Control.Monad.State    (get, put)
import           Data.Acid
import           Data.Function          (on)
import           Data.List              (deleteFirstsBy)
import           Data.SafeCopy
import           Money                  (Transaction, amount, date, description)
import           Scrapers               (Cred (..), Credential (..))

type DBContext = AcidState Database
data Database = Database [Transaction] [Credential]

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''Transaction)
$(deriveSafeCopy 0 'base ''Credential)
$(deriveSafeCopy 0 'base ''Cred)

-- returns the number of new transactions (those that are not already in the db)
mergeTransactions_ :: [Transaction] -> Update Database Int
mergeTransactions_ new = do
    Database old creds <- get
    let updated = mergeTransactionsPure new old
    put $ Database updated creds
    return (length updated - length old)

mergeTransactionsPure :: [Transaction] -> [Transaction] -> [Transaction]
mergeTransactionsPure new old = old ++ deleteFirstsBy eqOnAllButTags new old
   where
       eqOnAllButTags = and2 [(==) `on` date, (==) `on` description, (==) `on` amount]
       and2 :: [a -> a -> Bool] -> a -> a -> Bool
       and2 fs a b = all (\f -> f a b) fs

getTransactions_ :: Query Database [Transaction]
getTransactions_ = do
    Database ts _ <- ask
    return ts

addCredential_ :: Credential -> Update Database ()
addCredential_ cred = do
    Database ts creds <- get
    put $ Database ts (cred:creds)

getCredentials_ :: Query Database [Credential]
getCredentials_ = do
    Database _ creds <- ask
    return creds

$(makeAcidic ''Database ['mergeTransactions_, 'getTransactions_, 'addCredential_, 'getCredentials_])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

getTransactions :: DB [Transaction]
getTransactions = do
    database <- ask
    liftIO $ query database GetTransactions_

mergeTransactions :: [Transaction] -> DB Int
mergeTransactions new = do
    database <- ask
    liftIO $ update database (MergeTransactions_ new)

addCredential :: Credential -> DB ()
addCredential cred = do
    database <- ask
    liftIO $ update database (AddCredential_ cred)

getCredentials :: DB [Credential]
getCredentials = do
    database <- ask
    liftIO $ query database GetCredentials_

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database [] [])
