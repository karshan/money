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
    , runDB
    , openDB
    , getTransactions
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

type DBContext = AcidState Database
data Database = Database [Transaction]

$(deriveSafeCopy 0 'base ''Database)
$(deriveSafeCopy 0 'base ''Transaction)

mergeTransactions_ :: [Transaction] -> Update Database ()
mergeTransactions_ new
    = do Database old <- get
         put $ Database (mergeTransactionsPure new old)

mergeTransactionsPure :: [Transaction] -> [Transaction] -> [Transaction]
mergeTransactionsPure new old = old ++ deleteFirstsBy eqOnAllButTags new old
   where
       eqOnAllButTags = and2 [(==) `on` date, (==) `on` description, (==) `on` amount]
       and2 :: [a -> a -> Bool] -> a -> a -> Bool
       and2 fs a b = all (\f -> f a b) fs

getTransactions_ :: Query Database [Transaction]
getTransactions_
    = do Database ts <- ask
         return ts

-- This will define @ViewMessage@ and @AddMessage@ for us.
$(makeAcidic ''Database ['mergeTransactions_, 'getTransactions_])

newtype DB a = DB { unDB :: ReaderT (AcidState Database) IO a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader (AcidState Database))

getTransactions :: DB [Transaction]
getTransactions = do
    database <- ask
    liftIO $ query database GetTransactions_

mergeTransactions :: [Transaction] -> DB ()
mergeTransactions new = do
    database <- ask
    liftIO $ update database (MergeTransactions_ new)

runDB :: DBContext -> DB a -> IO a
runDB db (DB a) = runReaderT a db

openDB :: FilePath -> IO DBContext
openDB fp = openLocalStateFrom fp (Database [])
